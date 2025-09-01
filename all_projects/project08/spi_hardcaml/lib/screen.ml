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
      ; counter : 'a [@bits 33]  (* For debugging purposes *)
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init_power | Send_data
    [@@deriving sexp_of, compare, enumerate]
  end
  
  let command_rom ~index =
    let open Signal in
    let commands = [0x01; 0x11; 0x3A; 0x29] in
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

  
    let cs = Variable.wire ~default:vdd in
    let reset = Variable.wire ~default:vdd in

    let sclk = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let sdin = Variable.wire ~default:gnd in
    let dc = Variable.reg ~enable:vdd reg_sync_spec ~width:1 in
    let command_index = Variable.reg ~enable:vdd reg_sync_spec ~width:4 in

    let column_index = Variable.reg ~enable:vdd reg_sync_spec ~width:10 in

    let dataToSend = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let bitCounter = Variable.reg ~enable:vdd reg_sync_spec ~width:4 in
    (* The program block with a call to [compile]. *)
    compile [
      sm.switch [
        (Init_power,   [sclk <--. 1; cs <--. 0; dc <--. 0;
                         if_ (counter <:. X.startup_wait) [reset <--. 1][reset <--. 0];
                         when_ (counter >:. (X.startup_wait * 3)) [reset <--. 1; bitCounter <--. 0; sm.set_next Send_data]]);
        (Send_data, [
                      when_ (bitCounter.value ==: (of_int ~width:4 0))
                        [
                          when_ (dc.value ==: gnd) 
                            [dataToSend <-- command_rom ~index:command_index.value;
                             command_index <-- (command_index.value +:. 1);
                             when_ (command_index.value ==: of_int ~width:4 3)
                               [command_index <--. 0; dc <--. 1;];
                            ];
                          when_ (dc.value ==: vdd)
                            [dataToSend <-- display_rom ~index:column_index.value;
                             column_index <-- (column_index.value +:. 1);
                             when_ (column_index.value ==: of_int ~width:10 (128*160 - 1))
                               [column_index <--. 0;];
                            ];
                          bitCounter <--. 0;
                        ];
                      when_ (bitCounter.value <: (of_int ~width:4 8))
                        [ reset <--. 0; cs <--. 0; dc <--. 1; sm.set_next Send_data;];
                      when_ (bitCounter.value ==: (of_int ~width:4 8))
                        [ reset <--. 0];
                      sclk <-- ~:(sclk.value);
                      bitCounter <-- (bitCounter.value +:. 1);
                      sdin <-- gnd
                    ]);  
      ]
    ];
    {O.io_sclk = sclk.value; io_sdin = sdin.value; io_cs = cs.value; io_dc = dc.value; io_reset = reset.value; counter = counter}

end