(* gowin_oser10.ml *)
open Hardcaml

module I = struct
  type 'a t =
    { pclk : 'a [@rtlname "PCLK"]
    ; fclk : 'a [@rtlname "FCLK"]
    ; reset : 'a [@rtlname "RESET"]
    ; d0 : 'a [@rtlname "D0"]
    ; d1 : 'a [@rtlname "D1"]
    ; d2 : 'a [@rtlname "D2"]
    ; d3 : 'a [@rtlname "D3"]
    ; d4 : 'a [@rtlname "D4"]
    ; d5 : 'a [@rtlname "D5"]
    ; d6 : 'a [@rtlname "D6"]
    ; d7 : 'a [@rtlname "D7"]
    ; d8 : 'a [@rtlname "D8"]
    ; d9 : 'a [@rtlname "D9"]
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { q : 'a [@rtlname "Q"]
    }
  [@@deriving hardcaml]
end

let parameters = 
  List.map
  (fun (name, value) -> Parameter.create ~name ~value)
  [
    "LSREN", Parameter.Value.Bool true; (* Local Set/Reset Enable *)
    "GSREN", Parameter.Value.Bool false; (* Global Set/Reset Enable *)
  ]

let create (_scope : Scope.t) (i : _ I.t) =
  let module Inst = Instantiation.With_interface(I)(O) in
  Inst.create ~name:"OSER10" ~instance:"inst" ~parameters i

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"gowin_oser10" ~instance:"inst" create i