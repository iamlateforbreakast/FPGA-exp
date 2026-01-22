(* top.ml *)
(* screen.ml *)
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
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = INIT | SEND_CMD | WAIT_SPI_CMD | SEND_DATA | WAIT_SPI_DATA
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
    (* let { I.clock; i_reset } = i in *)
    (* Create synchronous registers *)
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:i.i_reset () in

    (* State machine and Registers *)
    let sm = State_machine.create (module States) reg_sync_spec ~enable:vdd in
    let cmd_idx = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let data_idx = Variable.reg ~enable:vdd reg_sync_spec ~width:13 in (* 128*8 = 1024 *)
    let dc_reg = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in

    (* Mux between Command ROM and Data ROM based on state *)
    let current_data = wire 8 in

    (* Instantiate the screen SPI controller *)
    let screen_spi = MyScreen.hierarchical scope (
      MyScreen.I.{ clock = i.clock; reset = i.i_reset; data_in = current_data }
    ) in

    (* Logic for SPI "done" signal - Assuming the SPI controller has a way to signal completion.
       If your screen_spi doesn't have a 'done' output, you would typically add one. *)
    let spi_done = wire 1 in (* Placeholder: replace with actual 'done' signal if available *)

    compile [
      sm.switch [
        INIT, [
          cmd_idx   <--. 0;
          data_idx  <--. 0;
          sm.set_next SEND_CMD;
        ];
        
        SEND_CMD, [
          dc_reg $== gnd;
          current_data <-- (command_rom cmd_idx);
          sm.set_next WAIT_SPI_CMD;
        ];
        
        WAIT_SPI_CMD, [
          if_ spi_done [
            if_ (cmd_idx.value ==:. (List.length X.commands - 1)) [
              sm.set_next SEND_DATA;
            ] [
              cmd_idx <--. cmd_idx.value +:. 1;
              sm.set_next SEND_CMD;
            ]
          ]
        ];
        
        SEND_DATA, [
          dc_reg $== vdd;
          sm.set_next WAIT_SPI_DATA;
        ];
        
        WAIT_SPI_DATA, [
          if_ spi_done [
            if_ (data_idx.value ==:. (128 * 8 - 1)) [
              sm.set_next INIT; (* Loop back or go to IDLE *)
            ] [
              data_idx <--. data_idx.value +:. 1;
              sm.set_next SEND_DATA;
            ]
          ]
        ];
      ]
    ];
    
    { O.o_sclk  = screen_spi.sclk
    ; O.o_sdin  = screen_spi.mosi
    ; O.o_cs    = screen_spi.cs
    ; O.o_dc    = dc_reg.value
    ; O.o_reset = ~: (i.i_reset) (* Standard active-low reset for screens *)
    }

end
