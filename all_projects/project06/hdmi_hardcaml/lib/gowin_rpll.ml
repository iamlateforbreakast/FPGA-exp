(* gowin_rpll.ml *)
open Hardcaml

module I = struct
  type 'a t =
    { clkin : 'a
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { lock : 'a
    ; clkout : 'a
    }
  [@@deriving hardcaml]
end

module Circuit = Circuit.With_interface(I)(O)

let parameters = 
  List.map
  (fun (name, value) -> Parameter.create ~name ~value)
  [
    "FCLKIN", Parameter.Value.Int 27;
    "DYN_IDIV_SEL", Parameter.Value.Bool false;  (* Dynamic Input Divider Selection *)
    "IDIV_SEL", Parameter.Value.Int 3;   (* Input Clock Divider *)
    "DYN_FBDIV_SEL", Parameter.Value.Bool false;  (* Dynamic Feedback Divider Selection *)
    "FBDIV_SEL", Parameter.Value.Int 54;  (* Feedback Clock Divider *)
    "DYN_ODIV_SEL", Parameter.Value.Bool false;  (* Dynamic Output Divider Selection *)  
    "ODIV_SEL", Parameter.Value.Int 2;   (* Output Clock Divider *)
    "PSDA_SEL", Parameter.Value.Int 1b'0000;   (* Phase Shift Selection *)
    "DYN_DA_EN", Parameter.Value.Bool true;  (* Dynamic Phase Shift Enable *)
    "DUTYDA_SEL", Parameter.Value.Int 1b'1000; (* Duty Cycle Correction Selection *)
    "CLKOUT_FT_DIR", Parameter.Value.Bool 1'b1;  (* CLKOUT Falling Edge Direction *)
    "CLKOUTP_FT_DIR", Parameter.Value.Int 1'b1; (* CLKOUTP Falling Edge Direction *)
    "CLKOUT_DLY_STEP", Parameter.Value.Int 0;  (* CLKOUT Delay Step *)
    "CLKOUTP_DLY_STEP", Parameter.Value.Int 0; (* CLKOUTP Delay Step *)
    "CLKFB_SEL", Parameter.Value.String "internal";  (* Feedback Clock *)
    "CLKOUT_BYPASS", Parameter.Value.Bool false;  (* CLKOUT Bypass *)
    "CLKOUTP_BYPASS", Parameter.Value.Bool false; (* CLKOUTP Bypass *)
    "DYN_SDIV_SEL", Parameter.Value.Int 2;  (* Dynamic Secondary Divider Selection *)
    "CLKOUTD_SRC", Parameter.Value.String "CLKOUT";  (* Secondary Output Clock Source *)
    "CLKOUTD3_SRC", Parameter.Value.String "CLKOUT"; (* Tertiary Output Clock Source *)
    "DEVICE", Parameter.Value.String "GW2AR-18C";  (* Target Device *)
  ]

let create (_scope : Scope.t) (i : _ I.t) =
  let module Inst = Instantiation.With_interface(I)(O) in
  Inst.create ~name:"rPLL" ~instance:"inst" ~parameters i

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"gowin_rpll" ~instance:"inst1" create i
