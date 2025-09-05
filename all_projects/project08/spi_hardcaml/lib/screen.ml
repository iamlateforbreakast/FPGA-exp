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
      ~width:33 
      ~f:(fun c -> (c +:. 1)) in
    let clk_counter = reg_fb reg_sync_spec
      ~enable:vdd 
      ~width:4 
      ~f:(fun c -> mux2 (c ==:. 2)(zero 4)(c +:. 1)) in
    let clk3 = reg_fb reg_sync_spec
      ~enable:vdd 
      ~width:1
      ~f:(fun _ -> mux2 (clk_counter ==:. 0)(vdd)(gnd)) in
    let sm = Always.State_machine.create (module States) reg_sync_spec ~enable:clk3 in
    (* Outputs *)
    (* Registers *)
  
    let cs = Variable.reg ~enable:clk3 reg_sync_spec ~width:1 in
    let _dbg_cs = Signal.(cs.value -- "dbg_cs") in
    let reset = Variable.reg ~enable:i.clock reg_sync_spec ~width:1 in
    let sdin = Variable.reg ~enable:clk3 reg_sync_spec ~width:1 in
    let dc = Variable.reg ~enable:clk3 reg_sync_spec ~width:1 in
    let command_index = Variable.reg ~enable:clk3 reg_sync_spec ~width:4 in
    let _dbg_command_index = Signal.(command_index.value -- "dbg_command_index") in
    let column_index = Variable.reg ~enable:clk3 reg_sync_spec ~width:10 in
    let _dbg_column_index = Signal.(column_index.value -- "dbg_column_index") in
    let data_to_send = Variable.reg ~enable:clk3 reg_sync_spec ~width:8 in
    let _dbg_data_to_send = Signal.(data_to_send.value -- "dbg_data_to_send") in
    let bit_counter = Variable.reg ~enable:clk3 reg_sync_spec ~width:4 in
    let _dbg_bit_counter = Signal.(bit_counter.value -- "dbg_bit_counter") in
    (* The program block with a call to [compile]. *)
    compile [
      sm.switch [
        (Init_power, [reset <-- gnd; cs <--. 1; dc <--. 0;
                         if_ (counter <=:. X.startup_wait) [reset <-- gnd][reset <-- vdd];
                         when_ (counter >:. (X.startup_wait * 3)) 
                           [ reset <-- gnd
                           ; cs <--. 0
                           ; sm.set_next Load_command]]);
        (Load_command, [ bit_counter <--. 0
                       ; if_ (command_index.value <=: (of_int ~width:4 (List.length X.commands))) 
                           [dc <--. 0;  data_to_send <-- command_rom ~index:command_index.value; sm.set_next Send_data;]
                           [sm.set_next Load_display]]);
        (Load_display, [ bit_counter <-- (of_int ~width:4 0)
                       ; if_ (column_index.value <=: of_int ~width:10 (128*8 - 1))
                           [dc <--. 1; data_to_send <-- display_rom ~index:column_index.value; sm.set_next Send_data;]
                           [column_index <--. 0; sm.set_next Load_display]]);
        (Send_data,   [ bit_counter <-- (bit_counter.value +:. 1);
                      sdin <-- bit data_to_send.value 7;
                      data_to_send <-- sll data_to_send.value 1;
                      when_ (bit_counter.value ==: (of_int ~width:4 8)) 
                        [ bit_counter <-- (of_int ~width:4 0); sm.set_next Select_data]]);
        (Select_data, [when_ (dc.value ==: vdd) [ column_index <-- (column_index.value +:. 1); sm.set_next Load_display ];
                      when_ (dc.value ==: gnd) [ command_index <-- (command_index.value +:. 1); sm.set_next Load_command ];
                      ]);  
      ]
    ];
    { O.io_sclk = clk3
    ; O.io_sdin = bit data_to_send.value 7
    ; O.io_cs = cs.value
    ; O.io_dc = dc.value
    ; O.io_reset = reset.value
    }

end
