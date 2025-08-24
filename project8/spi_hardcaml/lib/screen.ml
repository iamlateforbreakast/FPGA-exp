(* screen.ml *)
open Hardcaml

module type Config = Config_intf.S

module Make (X : Config) = struct

  let startup_wait_parameter = Parameter.create ~name:"STARTUP_WAIT" ~value:(Parameter.Value.Int 10_000_000)

  module I = struct
    type 'a t =
      { i_clk : 'a
      ; i_reset : 'a
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
  
  let create (_scope: Scope.t) (i: _ I.t) =
    let open Signal in
    (* Create synchronous registers *)
    let reg_sync_spec = Reg_spec.create ~clock:i.i_clk ~clear:gnd () in


    (*
    let dc =
    let sclk =
    let sdin =
    let cs =
    *)
    
    (* State machine definition *)
    let sm = Always.State_machine.create (module States) reg_sync_spec ~enable:vdd in
    let counter = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:33 
      ~f:(fun c -> mux2 (c <:. 30_000_000)(zero 33)(c +:. 1)) in
    (* let reset = Always.Variable.wire ~default:vdd in *)
    (* Signal.set_names reset.value ["i_reset"]; *)

    (* print_endline (reset.value |> Signal.to_string); *)
    let out = Always.Variable.reg reg_sync_spec ~width:1 ~enable:vdd in

    (* The program block with a call to [compile]. *)
    Always.(compile [
      sm.switch [
        (Init_power, [if_ (counter <:. 10_000_000) [out <-- vdd][out <-- gnd]]);
        (Send_command, []);
        (Load_data, []);
      ]
    ]);
    {O.o_sclk = i.i_clk; o_sdin = Signal.gnd; o_cs = Signal.gnd; o_dc = gnd; o_reset = bit counter 0}

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"screen" ~instance:"inst1" create i
end

