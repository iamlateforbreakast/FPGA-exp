(* gowin_oser10.ml *)
open Hardcaml

module I = struct
  type 'a t =
    { input : 'a [@bits 10, @name "D"]
    ; fclk : 'a [@name "FCLK"]
    ; pclk : 'a [@name "PCLK"]
    ; reset : 'a [@name "RESET"]
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { output : 'a [@name "Q"]
    }
  [@@deriving hardcaml]
end

let create (_scope : Scope.t) (_i : _ I.t) =
  { O.output = Signal.gnd; O.output_b = Signal.gnd }

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"OSER10" ~instance:"inst1" create i
