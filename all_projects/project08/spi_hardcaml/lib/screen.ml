(* screen.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config) = struct

  (* let startup_wait_parameter = Parameter.create ~name:"STARTUP_WAIT" ~value:(Parameter.Value.Int 10_000_000)
  *)
  
  module I = struct
    type 'a t =
      { i_clk : 'a [@bits 1]
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
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init_power | Load_command | Load_display | Send_data
    [@@deriving sexp_of, compare, enumerate]
  end
    
  (*
  reg [7:0] dataToSend = 0;
  reg [3:0] bitNumber = 0;  
  reg [9:0] pixelCounter = 0;
  *)
  
  let create (_scope: Scope.t) (i: _ I.t) : _ O.t =
    let open Always in
    let open Signal in
    let { I.i_clk; i_reset } = i in
    (* Create synchronous registers *)
    let reg_sync_spec = Reg_spec.create ~clock:i_clk ~clear:i_reset () in
(*
    let counter = reg ~width:33 clk in
    let state = reg ~width:State.width clk in
    let data_to_send = reg ~width:8 clk in
    let bit_number = reg ~width:4 clk in
    let pixel_counter = reg ~width:10 clk in
  *)

    (* State machine definition *)
    let sm = Always.State_machine.create (module States) reg_sync_spec ~enable:vdd in
    let counter = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:33 
      ~f:(fun c -> mux2 (c <:. 30_000_000)(zero 33)(c +:. 1)) in

    let sclk = Variable.wire ~default:gnd in
    let cs = Variable.wire ~default:vdd in
    let dc = Variable.wire ~default:gnd in
    let reset = Variable.wire ~default:vdd in

    let dataToSend = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let bitCounter = Variable.reg ~enable:vdd reg_sync_spec ~width:3 in 
    (* The program block with a call to [compile]. *)
    compile [
      sm.switch [
        (Init_power, [if_ (counter <:. 10_000_000) [reset <--. 1][if_ (counter <:. 20_000_000) [reset <--. 0][reset <--. 1]]]);
        (Load_command, [reset <--. 1; dc <--. 0; sm.set_next Load_data;]);
        (Load_display, [reset<--. 1; dc <--. 1; sm.set_next Send_command]);
        (Send_data, [reset<--. 1; cs <--. 1; sm.set_next Send_command]);
      ]
    ];
    {O.io_sclk = sclk.value; io_sdin = Signal.gnd; io_cs = cs.value; io_dc = dc.value; io_reset = reset.value}

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"screen" create i
end

