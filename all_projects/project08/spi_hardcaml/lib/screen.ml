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
      { io_sclk :  'a [@bits 1]
      ; io_sdin :  'a [@bits 1]
      ; io_cs :    'a [@bits 1]
      ; io_dc :    'a [@bits 1]
      ; io_reset : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init_power | Load_command | Load_display | Send_data | Select_data
    [@@deriving sexp_of, compare, enumerate]
  end
  
  let command_rom ~index =
    let open Signal in
    let rom = List.map (fun c -> of_int ~width:8 c) X.commands in
    mux index rom

  let display_rom_alternate ~clock ~read_address =
    let open Signal in
    (* 1. Prepare initial data (128 * 8 elements) *)
    let initial_data = Array.init (128 * 8) (fun i -> 
      of_int ~width:8 (i mod 256)) in

    (* 2. Define the Read Port *)
    let read_port = 
      { read_clock = clock
      ; read_address = read_address
      ; read_enable = vdd
      } 
    in

    (* 3. Create the memory. For a ROM, write_ports is an empty array. *)
    (* initialize_with is often a parameter or a specialized constructor like Ram.create *)
    let outputs = multiport_memory 1024
      ~write_ports:[||]
      ~read_ports:[| read_port |]
    in
    outputs.(0) (* Return data from the first (only) read port *)
  
  let display_rom ~index =
    let open Signal in
    let size = 128 * 8 in
    let rom = List.init size (fun i -> of_int ~width:8 (i mod 256)) in
    mux index rom
  
  let create (_scope: Scope.t) (i: _ I.t) : _ O.t =
    let open Always in
    let open Signal in
    (* let { I.clock; i_reset } = i in *)
    (* Create synchronous registers *)
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:i.i_reset () in

    (* State machine definition *)
    
    let counter = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:16 
      ~f:(fun c -> mux2 (c <:. (X.clk_div * 2 - 1)) (c +:. 1)(zero 16)) in
    let _dbg_counter = Signal.(counter -- "dbg_counter") in
    
    let sm = Always.State_machine.create (module States) reg_sync_spec ~enable:vdd in
    (* Outputs *)
    (* Registers *)
  
    let sclk = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let ncs = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let nreset = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let reset_counter = Variable.reg ~enable:vdd reg_sync_spec ~width:24 in
    let _dbg_reset_counter = Signal.(reset_counter.value -- "dbg_reset_counter") in
    let sdin = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let ndc = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let command_index = Variable.reg ~enable:vdd reg_sync_spec ~width:4 in
    let _dbg_command_index = Signal.(command_index.value -- "dbg_command_index") in
    let column_index = Variable.reg ~enable:vdd reg_sync_spec ~width:10 in
    let _dbg_column_index = Signal.(column_index.value -- "dbg_column_index") in
    let data_to_send = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let _dbg_data_to_send = Signal.(data_to_send.value -- "dbg_data_to_send") in
    let bit_counter = Variable.reg ~enable:vdd reg_sync_spec ~width:4 in
    let _dbg_bit_counter = Signal.(bit_counter.value -- "dbg_bit_counter") in
    (* The program block with a call to [compile]. *)
    compile [
      sm.switch [
        (Init_power, 
          [ sclk <-- vdd; sdin <-- vdd
          ; ncs <-- gnd; reset_counter <-- (reset_counter.value +:. 1)
          ; when_ (reset_counter.value ==:. X.startup_wait) [nreset <-- vdd]
          ; when_ (reset_counter.value ==:. X.startup_wait * 2) [nreset <-- gnd;]
          ; when_ (reset_counter.value ==:. X.startup_wait * 3) [sdin <-- gnd; sm.set_next Load_command]]);
        (Load_command, 
          [ sclk <-- gnd; ncs <-- vdd; reset_counter <--. 0; bit_counter <--. 0
          ; if_ (command_index.value <=: (of_int ~width:4 (List.length X.commands - 1))) 
              [ndc <--. 1;  data_to_send <-- command_rom ~index:command_index.value; sm.set_next Send_data;]
              [sm.set_next Load_display]]);
        (Load_display, 
          [ ncs <-- vdd; bit_counter <-- (of_int ~width:4 0)
          ; if_ (column_index.value <=: of_int ~width:10 (128*8 - 1))
              [ndc <--. 0; data_to_send <-- display_rom ~index:column_index.value; sm.set_next Send_data;]
              [column_index <--. 0; sm.set_next Load_display]]);
        (Send_data,
          [ if_ (counter <=:. (X.clk_div - 1)) [sclk <-- gnd] [sclk <-- vdd]
          ; when_ (counter ==:. (X.clk_div - 2)) 
                [ bit_counter <-- (bit_counter.value +:. 1)
                ; sdin <-- bit data_to_send.value 7
                ; data_to_send <-- sll data_to_send.value 1
                ; when_ (bit_counter.value ==: (of_int ~width:4 8)) 
                  [ bit_counter <-- (of_int ~width:4 0); sm.set_next Select_data]]]);
        (Select_data, 
          [ if_ (counter <=:. (X.clk_div - 1)) [sclk <-- vdd] [sclk <-- gnd]
          ; sclk <-- gnd; ncs <-- gnd
          ; when_ (ndc.value ==: gnd)
              [ column_index <-- (column_index.value +:. 1); sm.set_next Load_display ]
          ; when_ (ndc.value ==: vdd) 
              [ command_index <-- (command_index.value +:. 1); sm.set_next Load_command ];
          ]);
      ]
    ];
    { O.io_sclk = sclk.value
    ; O.io_sdin = sdin.value
    ; O.io_cs = ~:(ncs.value)
    ; O.io_dc = ~:(ndc.value)
    ; O.io_reset = ~:(nreset.value)
    }

end
