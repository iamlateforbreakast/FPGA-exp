(* svo_hdmi.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clk : 'a
      ; resetn : 'a
      ; out_axis_tready : 'a
      ; clk_pixel : 'a
	    ; clk_5x_pixel : 'a
	    ; locked : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { tmds_clk_n : 'a
      ; tmds_clk_p : 'a
      ; tmds_d_n : 'a [@bits 3]
      ; tmds_d_p : 'a [@bits 3]
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (_i : _ I.t) =
    { O.tmds_clk_n = Signal.gnd;
      O.tmds_clk_p = Signal.gnd;
      O.tmds_d_n = Signal.gnd @: Signal.gnd @: Signal.gnd; 
      O.tmds_d_p = Signal.gnd @: Signal.gnd @: Signal.gnd
    }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"svo_hdmi" ~instance:"inst1" create i
end
