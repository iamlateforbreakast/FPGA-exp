(* svo_tcard.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clk : 'a
      ; resetn : 'a
      ; out_axis_tready : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { out_axis_tvalid : 'a
      ; out_axis_tdata : 'a [@bits X.svo_bits_per_pixel]
      ; out_axis_tuser : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (_i : _ I.t) =
    { O.out_axis_tvalid = Signal.gnd; O.out_axis_tdata = Signal.gnd; O.out_axis_tuser = Signal.gnd }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"svo_tcard" ~instance:"inst1" create i
end
