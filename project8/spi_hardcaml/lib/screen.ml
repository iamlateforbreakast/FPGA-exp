(* screen.ml *)
open Hardcaml

module type Config = Config_intf.S

module Make (X : Config) = struct

  let startup_wait_parameter = Parameter.create ~name:"STARTUP_WAIT" ~value:(Parameter.Value.Int 10_000_000)

  module I = struct
    type 'a t =
      { clk : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { io_sclk : 'a
      ; io_sdin : 'a
      ; io_cs : 'a
      ; io_dc : 'a
      ; io_reset : 'a
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init_power | Send_command | Load_data
    [@@deriving sexp_of, compare, enumerate]
  end

  (* Init counter *)
  reg [32:0] counter = 0;
  let spec = Reg_spec.create ~clock ()
  
  let counter = Reg.reg_fb spec ~enable:vdd
  let dc =
  let sclk = Reg.reg_fb spec
  let sdin
  let reset
  let cs
  (*
  
  reg [7:0] dataToSend = 0;
  reg [3:0] bitNumber = 0;  
  reg [9:0] pixelCounter = 0;
  *)
  
  (* State machine definition *)
  let sm_spec = Reg_spec.create ~clock () in
    let sm = Always.State_machine.create (module States) sm_spec ~enable:vdd
  
  
  let create (_scope: Scope.t) (_i: _ I.t) =
    let open Signal in
    
    let c_wire = Always.Variable.wire ~default:(Signal.zero 8) () in
    let c_reg = Always.Variable.reg ~enable:Signal.vdd r_sync ~width:8 in
  
    (* The program block with a call to [compile]. *)
    Always.(compile [
      if_ (a ==: b) [
        c_wire <-- (sll a ~by:1);
        c_reg  <-- (sll a ~by:1)
      ] [
        c_wire <-- (a +: b);
        c_reg  <-- (a +: b);
      ]
    ]);
    output "c_wire" c_wire.value, output "c_reg" c_reg.value
end
