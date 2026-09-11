(* top.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
    type 'a t =
      { clock :    'a [@bits 1]  (* Need to be called clock for simulation *)
      ; i_reset :  'a [@bits 1]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_sclk :  'a [@bits 1]
      ; o_sdin :  'a [@bits 1]
      ; o_cs :    'a [@bits 1]
      ; o_dc :    'a [@bits 1]
      ; o_reset : 'a [@bits 1]
      ; o_led :   'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = INIT | SEND_CMD | WAIT_SPI_CMD | PAGE_SETUP | WAIT_SPI_PAGE_SETUP
           | SEND_DATA | WAIT_SPI_DATA
    [@@deriving sexp_of, compare, enumerate]
  end

  let command_rom ~index =
    let open Signal in
    let rom = List.map (fun c -> of_int ~width:8 c) X.commands in
    mux index rom

  let display_rom ~index =
    let open Signal in
    let size = 128 * 8 in
    let rom = List.init size (fun i -> of_int ~width:8 (i mod 256)) in
    mux index rom

  (* SH1106 needs the page (0xB0..0xB7) and lower/upper column-address
     commands re-sent before every 128-byte page of data; the column
     address is constant (X.col_offset) since we always start each page at
     column 0 of the visible 128. *)
  let page_setup_rom ~setup_idx ~page =
    let open Signal in
    let page_cmd = of_int ~width:8 0xB0 |: uresize page 8 in
    let low_col = of_int ~width:8 (0x00 lor (X.col_offset land 0x0F)) in
    let high_col = of_int ~width:8 (0x10 lor (X.col_offset lsr 4)) in
    mux setup_idx [ page_cmd; low_col; high_col ]

  (*
  let display_rom (clock: Signal.t) (read_address: Signal.t) =
    let open Signal in
    (* 1. Prepare initial data (128 * 8 elements) *)
    let _initial_data = Array.init (128 * 8) (fun i -> 
      of_int ~width:8 (i mod 256)) in

    (* 3. Create the memory. For a ROM, write_ports is an empty array. *)
    (* initialize_with is often a parameter or a specialized constructor like Ram.create *)
    let outputs = Ram.create
      ~collision_mode:Write_before_read
      ~size:1024
      ~write_ports:[| |]
      ~read_ports:[| {read_clock = clock; read_address = read_address; read_enable = vdd} |]
      ()
    in
    outputs.(0) (* Return data from the first (only) read port *)
  *)
  module MyScreen = Screen_spi.Make(X)

  let create (scope: Scope.t) (i: _ I.t) : _ O.t =
    let open Always in
    let open Signal in
    (* Power-on reset. [por_cnt] deliberately has no reset of its own: it
       relies on Gowin's GSR clearing every flop to 0 at the end of
       configuration, then counts up and saturates once its top bit sets, so
       [por] is asserted for the first 256 cycles after configuration and
       never again.

       This is what gives the state machine below a defined starting state.
       Without it the state register has no initialiser at all, and a 9-state
       machine binary-encoded in 4 bits can come up in one of the unused
       encodings 9..15 - where no switch case matches, nothing is ever
       assigned, and the machine is stuck forever with every [sm.is] reading
       low. That is not hypothetical: it is exactly the failure this replaces,
       introduced by wiring [clear] to a constant to avoid depending on the
       board's marginal 1.8V reset button.

       The button is still ORed in, so boards whose button is electrically
       sound (the Nano 20K) keep a working manual reset, while the 4K - whose
       [normalize_reset] discards the pin - relies on the power-on path alone. *)
    let por_cnt =
      reg_fb (Reg_spec.create ~clock:i.clock ()) ~enable:vdd ~width:9
        ~f:(fun c -> mux2 (msb c) c (c +:. 1))
    in
    let por = ~:(msb por_cnt) in
    (* Board-specific button polarity is normalized here; everything below
       treats [reset] as active-high. *)
    let reset = X.normalize_reset i.i_reset |: por in
    (* Create synchronous registers *)
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:reset () in

    (* State machine and Registers *)
    let sm = State_machine.create (module States) reg_sync_spec ~enable:vdd in
    let cmd_idx = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let page_idx = Variable.reg ~enable:vdd reg_sync_spec ~width:3 in (* 0..7 *)
    let col_idx = Variable.reg ~enable:vdd reg_sync_spec ~width:7 in (* 0..127 *)
    let setup_idx = Variable.reg ~enable:vdd reg_sync_spec ~width:2 in (* 0..2 *)
    let dc_reg = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    (* Heartbeat for the onboard LED: a free-running counter whose top bit
       toggles roughly every 0.3s at 27MHz. This is the bring-up canary - if
       it blinks, the bitstream is loaded, the clock reaches the fabric and
       the design is out of reset. If it doesn't, nothing downstream (SPI,
       OLED) can possibly work, and the fault is here rather than in the
       peripheral. Deliberately independent of the SPI state machine, so it
       keeps blinking even if that machine stalls. *)
    let heartbeat = reg_fb reg_sync_spec ~enable:vdd ~width:24 ~f:(fun c -> c +:. 1) in

    (* The OLED's RES pulse is generated here, deliberately OUTSIDE the command
       state machine: a counter that saturates, holding RES low for the first
       2^19 cycles (~19ms at 27MHz) after reset and then releasing it forever.

       An earlier version drove RES from two extra leading FSM states. That
       coupled panel reset to command sequencing for no benefit and made the
       machine's start-up unobservable; it also meant any FSM stall left the
       panel pinned in reset. Keeping it separate restores the simpler command
       sequence that was measured working on this board, and guarantees the
       panel sees exactly one clean reset pulse regardless of what the FSM
       does afterwards - including looping back to INIT, which must NOT
       re-reset the panel. *)
    let oled_rst_cnt =
      reg_fb reg_sync_spec ~enable:vdd ~width:20
        ~f:(fun c -> mux2 (msb c) c (c +:. 1))
    in
    let oled_rst_n = msb oled_rst_cnt in

    (* Mux between Command ROM and Data ROM based on state *)
    let current_data = Variable.wire ~default:(zero 8) in

    (* Instantiate the screen SPI controller *)
    let screen_spi = MyScreen.hierarchical scope (
      MyScreen.I.{ clock = i.clock
                 ; reset
                 ; data_in = Always.Variable.value current_data
                 ; data_valid = (sm.is SEND_DATA |: sm.is SEND_CMD |: sm.is PAGE_SETUP)
                 }
    ) in

    compile [
      sm.switch [
        INIT, [
          cmd_idx   <--. 0;
          page_idx  <--. 0;
          col_idx   <--. 0;
          setup_idx <--. 0;
          sm.set_next SEND_CMD;
        ];

        SEND_CMD, [
          dc_reg <--. 0;
          current_data <-- (command_rom ~index:cmd_idx.value);
          sm.set_next WAIT_SPI_CMD;
        ];

        WAIT_SPI_CMD, [
          if_ screen_spi.ready [
            if_ (cmd_idx.value ==:. (List.length X.commands - 1)) [
              sm.set_next PAGE_SETUP;
            ] [
              cmd_idx <-- (cmd_idx.value +:. 1);
              sm.set_next SEND_CMD;
            ]
          ][]
        ];

        PAGE_SETUP, [
          dc_reg <--. 0;
          current_data <-- (page_setup_rom ~setup_idx:setup_idx.value ~page:page_idx.value);
          sm.set_next WAIT_SPI_PAGE_SETUP;
        ];

        WAIT_SPI_PAGE_SETUP, [
          if_ screen_spi.ready [
            if_ (setup_idx.value ==:. 2) [
              setup_idx <--. 0;
              sm.set_next SEND_DATA;
            ] [
              setup_idx <-- (setup_idx.value +:. 1);
              sm.set_next PAGE_SETUP;
            ]
          ][]
        ];

        SEND_DATA, [
          dc_reg <--. 1;
          current_data <-- (display_rom ~index:(concat_msb [ page_idx.value; col_idx.value ]));
          sm.set_next WAIT_SPI_DATA;
        ];

        WAIT_SPI_DATA, [
          if_ screen_spi.ready [
            if_ (col_idx.value ==:. 127) [
              col_idx <--. 0;
              if_ (page_idx.value ==:. 7) [
                page_idx <--. 0;
                sm.set_next INIT; (* Loop back or go to IDLE *)
              ] [
                page_idx <-- (page_idx.value +:. 1);
                sm.set_next PAGE_SETUP;
              ]
            ] [
              col_idx <-- (col_idx.value +:. 1);
              sm.set_next SEND_DATA;
            ]
          ][]
        ];
      ];

      (* Self-heal from the unused encodings. These nine states are binary
         encoded in four bits, so codes 9..15 exist but match no branch above:
         nothing is ever assigned in them, including the state itself, so the
         machine latches up permanently the instant it lands in one. Recovering
         explicitly costs one comparator and removes the possibility entirely,
         rather than leaving correctness resting on every flop powering up to a
         valid code. *)
      when_ (sm.current >=:. List.length States.all) [ sm.set_next INIT ];
    ];
    
    { O.o_sclk  = screen_spi.sclk
    ; O.o_sdin  = screen_spi.mosi
    ; O.o_cs    = screen_spi.cs
    ; O.o_dc    = dc_reg.value
    ; O.o_reset = oled_rst_n
    ; O.o_led   = msb heartbeat
    }

end
