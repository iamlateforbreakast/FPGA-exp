(* gowin_clkdiv.ml *)

(* gowin_clkdiv.ml *)
open Hardcaml

module I = struct
  type 'a t =
    { hclkin : 'a [@rtlname "HCLKIN"]
    ; resetn : 'a [@rtlname "RESETN"]
    ; calib : 'a [@rtlname "CALIB"]
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { clkout : 'a [@rtlname "CLKOUT"]
    }
  [@@deriving hardcaml]
end

(*
CLKDIV u_clkdiv
(.RESETN(hdmi4_rst_n)
,.HCLKIN(serial_clk) //clk  x5
,.CLKOUT(pix_clk)    //clk  x1
,.CALIB (1'b1)
);
defparam u_clkdiv.DIV_MODE="5";
defparam u_clkdiv.GSREN="false";
*)

let parameters = 
  List.map
  (fun (name, value) -> Parameter.create ~name ~value)
  [
    "DIV_MODE", Parameter.Value.Int 5;  (* Clock Divider *)
    "GSREN", Parameter.Value.Bool false;  (* Global Set/Reset Enable *)
  ]

  (*
let create (_scope : Scope.t) (_i : _ I.t) =
  { O.clkout = Signal.gnd } *)

let create (_scope : Scope.t) (i : _ I.t) =
  let module Inst = Instantiation.With_interface(I)(O) in
  Inst.create ~name:"CLK_DIV" ~parameters i

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"gowin_clkdiv" ~instance:"inst1" create i
