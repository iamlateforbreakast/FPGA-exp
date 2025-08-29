(* svo_enc.ml *)
open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clk : 'a
      ; resetn : 'a
      ; in_axis_tvalid : 'a
      ; in_axis_tdata : 'a [@bits X.svo_bits_per_pixel]
      ; in_axis_tuser : 'a [@bits 1]
      ; out_axis_tready : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { in_axis_tready : 'a
      ; out_axis_tvalid : 'a
      ; out_axis_tdata : 'a [@bits X.svo_bits_per_pixel]
      ; out_axis_tuser : 'a [@bits 4]
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (_i : _ I.t) =
    { O.in_axis_tready = Signal.gnd;
      O.out_axis_tvalid = Signal.gnd;
      O.out_axis_tdata = zero 24; 
      O.out_axis_tuser = zero 4 }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"svo_enc" ~instance:"inst1" create i
end
