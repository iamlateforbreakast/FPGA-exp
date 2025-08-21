(* screen.ml *)
open Hardcaml

module type Config = Config_intf.S

module Make (X : Config) = struct

  let startup_wait_parameter = Parameter.create ~name:"STARTUP_WAIT" ~value:(Parameter.Value.Int 10_000_000)

  module I = struct
    type 'a t =
      { i_clk : 'a
      ; i_reset: 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_sclk : 'a
      ; o_sdin : 'a
      ; o_cs : 'a
      ; o_dc : 'a
      ; o_reset : 'a
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init_power | Send_command | Load_data
    [@@deriving sexp_of, compare, enumerate]
  end
    
  (*
  
  reg [7:0] dataToSend = 0;
  reg [3:0] bitNumber = 0;  
  reg [9:0] pixelCounter = 0;
  *)
  
  
  
    let state = Always.State_machine.create (module States) sm_spec ~enable:vdd
  
  
  let create (_scope: Scope.t) (i: _ I.t) =
    let open Signal in
    let open Always in
    (* Create synchronous registers *)
    let reg_sync_spec = Reg_spec.create ~clock:i.clk ~clear:i.reset () in
    let counter = reg_fb reg_sync_spec ~enable:vdd ~width:33 ~f:(fun c -> c +:. 1) in
    (*
    let reset =
    let dc =
    let sclk =
    let sdin =
    let cs =
    *)
    
    (* State machine definition *)
    let sm_spec = Reg_spec.create ~clock () in
    let sm = Always.State_machine.create (module States) sm_spec ~enable:vdd in

  
    (* The program block with a call to [compile]. *)
    Always.(compile [
      match_ state [
        Init_power, [];
        Send_command, [];
        Load_data, [];
      ]
    ]);
    {O.o_sclk = Empty; o_sdin = Empty; o_cs = Empty; o_dc = Empty; o_reset = Empty}
end
