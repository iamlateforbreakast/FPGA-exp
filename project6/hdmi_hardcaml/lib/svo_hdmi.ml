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

  module Tcard = Svo_tcard.Make(X)
  module Encoder = Svo_enc.Make(X)
  module Tmds = Svo_tmds.Make(X)

  let create (scope : Scope.t) (i : _ I.t) =
    let tcard = Tcard.hierarchical scope (Tcard.I.{ resetn=i.resetn; clk=i.clk; out_axis_tready=Signal.vdd }) in
    let enc = Encoder.hierarchical scope (Encoder.I.{ clk=i.clk; resetn=i.resetn; out_axis_tready=i.out_axis_tready; clk_pixel=i.clk_pixel; clk_5x_pixel=i.clk_5x_pixel; locked=i.locked }) in
    let tmds = Tmds.hierarchical scope (Tmds.I.{ clk=i.clk; resetn=i.resetn; out_axis_tready=i.out_axis_tready; clk_pixel=i.clk_pixel; clk_5x_pixel=i.clk_5x_pixel; locked=i.locked }) in
    (* Connect the outputs of the encoder to the TMDS outputs *)
    {
      O.tmds_clk_n = tcard.tmds_clk_n;
      O.tmds_clk_p = tcard.tmds_clk_p;
      O.tmds_d_n = enc.tmds_d_n;
      O.tmds_d_p = enc.tmds_d_p;
    }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"svo_hdmi" ~instance:"inst1" create i
end
