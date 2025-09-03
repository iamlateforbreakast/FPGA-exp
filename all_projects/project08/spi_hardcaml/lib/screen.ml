(* screen.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config) = struct

  (* let startup_wait_parameter = Parameter.create ~name:"STARTUP_WAIT" ~value:(Parameter.Value.Int 10_000_000)
  *)
  
  module I = struct
    type 'a t =
      { clock : 'a [@bits 1]
      ; i_reset : 'a [@bits 1]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { io_sclk : 'a [@bits 1]
      ; io_sdin : 'a [@bits 1]
      ; io_cs : 'a [@bits 1]
      ; io_dc : 'a [@bits 1]
      ; io_reset : 'a [@bits 1]
      ; debug1 : 'a [@bits 4]  (* For debugging purposes *)
      ; debug2 : 'a [@bits 10]  (* For debugging purposes *)
      ; debug3 : 'a [@bits 8]  (* For debugging purposes *)
      ; debug4: 'a [@bits 4]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init_power | Send_data
    [@@deriving sexp_of, compare, enumerate]
  end
  
  let command_rom ~index =
    let open Signal in
    let commands = [0x55; 0x11; 0x3A; 0x29] in
    let rom = List.map (fun c -> of_int ~width:8 c) commands in
    
    mux index rom
  
  let display_rom ~index =
    let open Signal in
    let size = 128 * 8 in
    let rom = List.init size (fun i -> of_int ~width:8 (i mod 256)) in
    mux index rom
  
  let create (_scope: Scope.t) (i: _ I.t) : _ O.t =
    let open Always in
    let open Signal in
    let { I.clock; i_reset } = i in
    (* Create synchronous registers *)
    let reg_sync_spec = Reg_spec.create ~clock:clock ~clear:i_reset () in
  (*
    let state = reg ~width:State.width clk in
    let pixel_counter = reg ~width:10 clk in
  *)

    (* State machine definition *)
    let sm = Always.State_machine.create (module States) reg_sync_spec ~enable:vdd in
    let counter = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:33 
      ~f:(fun c -> (c +:. 1)) in

  
    let cs = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let reset = Variable.wire ~default:vdd in
    let sclk = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let sdin = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let dc = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let command_index = Variable.reg ~enable:vdd reg_sync_spec ~width:4 in

    let column_index = Variable.reg ~enable:vdd reg_sync_spec ~width:10 in

    let dataToSend = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let bitCounter = Variable.reg ~enable:vdd reg_sync_spec ~width:4 in
    (* The program block with a call to [compile]. *)
    compile [
      sm.switch [
        (Init_power, [sclk <--. 0; reset <-- vdd; cs <--. 0; dc <--. 0;
                         if_ (counter <=:. X.startup_wait) [reset <-- vdd][reset <-- gnd];
                         when_ (counter >:. (X.startup_wait * 3)) 
                           [ reset <-- vdd
                           ; bitCounter <--. 0
                           ; cs <--. 1
                           ; command_index <--. 0
                           ; column_index <--. 0
                           ; sm.set_next Send_data]]);
        (Send_data, [ when_ (bitCounter.value ==: (of_int ~width:4 0))
                        [
                          when_ (dc.value ==: gnd) 
                            [dataToSend <-- command_rom ~index:command_index.value;];
                          when_ (dc.value ==: vdd)
                            [dataToSend <-- display_rom ~index:column_index.value;];
                        ];
                      bitCounter <-- (bitCounter.value +:. 1);
                      sclk <-- ~:(sclk.value);
                      sdin <-- bit dataToSend.value 7;
                      dataToSend <-- mux2 (dc.value)(lsbs (display_rom ~index:column_index.value) @: zero 1)(lsbs (command_rom ~index:command_index.value) @: zero 1);
                      when_ (bitCounter.value ==: (of_int ~width:4 8))
                        [ when_ (dc.value ==: gnd)
                          [
                            if_ (command_index.value <=: (of_int ~width:4 2)) 
                              [command_index <-- (command_index.value +:. 1)][dc <--. 1;]];
                          when_ (dc.value ==: vdd)
                          [
                            if_ (column_index.value <=: of_int ~width:10 (128*8 - 1))
                              [column_index <-- (column_index.value +:. 1)][column_index <--. 0];
                          ];
                          bitCounter <-- (of_int ~width:4 0);
                        ];
                    ]);  
      ]
    ];
    { O.io_sclk = sclk.value
    ; O.io_sdin = sdin.value
    ; O.io_cs = cs.value
    ; O.io_dc = dc.value
    ; O.io_reset = reset.value
    ; O.debug1 = command_index.value
    ; O.debug2 = column_index.value
    ; O.debug3 = dataToSend.value
    ; O.debug4 = bitCounter.value}

end