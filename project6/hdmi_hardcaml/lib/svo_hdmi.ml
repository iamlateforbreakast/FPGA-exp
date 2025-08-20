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
    let tcard = Tcard.hierarchical scope (
	  Tcard.I.{ resetn=i.resetn
                  ; clk=i.clk
		  ; out_axis_tready=Signal.vdd }) in
    let enc = Encoder.hierarchical scope (
	  Encoder.I.{ clk=i.clk
                    ; resetn=i.resetn
		    ; out_axis_tready= Signal.gnd
                    ; in_axis_tvalid=tcard.out_axis_tvalid
                    ; in_axis_tdata=tcard.out_axis_tdata
                    ; in_axis_tuser=tcard.out_axis_tuser }) in

    let tmds_outputs = 
      Array.init 3 (fun idx ->
        Tmds.hierarchical scope (
	  Tmds.I.{ clk = i.clk_pixel
                 ; resetn = i.resetn
	         ; de = Signal.bit enc.out_axis_tuser 3
                 ; ctrl = Signal.select enc.out_axis_tuser 2 1
                 ; din = Signal.select enc.out_axis_tdata (23-idx*8) (16-idx*8) })) in

    let oser10_outputs =
      Array.init 3 (fun idx -> (
        Gowin_oser10.hierarchical scope 
          Gowin_oser10.I.{ pclk = i.clk_pixel
                         ; fclk = i.clk_5x_pixel
                         ; reset = i.resetn
                         ; inp =  tmds_outputs.(idx).dout })) in
    let buf_outputs =
      Array.init 4 (fun idx -> (
        Gowin_elvds_obuf.hierarchical scope { Gowin_elvds_obuf.I.inp =
          if idx = 0 then i.clk_pixel else oser10_outputs.(idx-1).out })) in

    let tmds_clk_p = buf_outputs.(0).out in
    let tmds_clk_n = buf_outputs.(0).out_b in
    let tmds_d_p = buf_outputs.(3).out @: buf_outputs.(2).out @: buf_outputs.(1).out in
    let tmds_d_n = buf_outputs.(3).out_b @: buf_outputs.(2).out_b @: buf_outputs.(1).out_b in
	
    (* Connect the outputs of the encoder to the TMDS outputs *)
    {
      O.tmds_clk_n = tmds_clk_n;
      O.tmds_clk_p = tmds_clk_p;
      O.tmds_d_n = tmds_d_n;
      O.tmds_d_p = tmds_d_p;
    }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"svo_hdmi" ~instance:"inst1" create i
end
