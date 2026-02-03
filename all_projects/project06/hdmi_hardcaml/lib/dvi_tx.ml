(* dvi_tx.ml *)
open Base
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t = {
      rst_n        : 'a;
      serial_clk   : 'a;
      rgb_clk      : 'a;
      rgb_vs       : 'a;
      rgb_hs       : 'a;
      rgb_de       : 'a;
      rgb_r        : 'a; [@bits 8]
      rgb_g        : 'a; [@bits 8]
      rgb_b        : 'a; [@bits 8]
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      tmds_clk_p   : 'a;
      tmds_clk_n   : 'a;
      tmds_data_p  : 'a; [@bits 3]
      tmds_data_n  : 'a; [@bits 3]
    } [@@deriving sexp_of, hardcaml]
  end

  module MyEncoder = Dvi_encoder.Make(X)

  (* Helper for OSER10 instantiation *)
  let oser10 ~inst ~data ~pclk ~fclk ~reset =
    let parameters = 
      List.map
      ~f:(fun (name, value) -> Parameter.create ~name ~value)
        [
          "GSREN", Parameter.Value.Bool false;
          "LSREN", Parameter.Value.Bool true;
        ] in
    Instantiation.create 
      ~name:"OSER10"
      ~instance:inst
      ~parameters:parameters
      ~inputs:[ "PCLK", pclk
              ; "FCLK", fclk
              ; "RESET", reset
              ; "D0", Signal.bit data 0
              ; "D1", Signal.bit data 1
              ; "D2", Signal.bit data 2
              ; "D3", Signal.bit data 3
              ; "D4", Signal.bit data 4
              ; "D5", Signal.bit data 5
              ; "D6", Signal.bit data 6
              ; "D7", Signal.bit data 7
              ; "D8", Signal.bit data 8
              ; "D9", Signal.bit data 9
              ] 
      ~outputs:[ "Q", 1 ]
      ()
    |> fun m -> Map.find_exn m "Q"

  (* Helper for ELVDS_OBUF instantiation *)
  let elvds_obuf ~inst ~input =
    let m = Instantiation.create
      ~name:"ELVDS_OBUF"
      ~instance:inst
      ~inputs:[ "I", input ]
      ~outputs:[ "O", 1; "OB", 1 ]
      ()
    in
    (Map.find_exn m "O", Map.find_exn m "OB")
  

  (* 1. DVI Encoders (Assumes an external dvi_encoder module exists) *)
  let encode ~scope ~inst ~data ~ctrl ~rst_n ~rgb_clk ~rgb_de=
    let encoder = MyEncoder.hierarchical scope inst
      (MyEncoder.I.
        { rst_n = rst_n
        ; pix_clk = rgb_clk
        ; de = rgb_de
        ; control = ctrl
        ; data = data
        })
  in
    encoder.encoded
  
  let create (scope : Scope.t)(i : Signal.t I.t) =
    let enc_b = encode ~scope ~inst:"blu" ~data:i.rgb_b ~ctrl:(concat_msb [i.rgb_vs; i.rgb_hs]) ~rst_n:i.rst_n ~rgb_clk:i.rgb_clk ~rgb_de:i.rgb_de in
    let enc_g = encode ~scope ~inst:"grn" ~data:i.rgb_g ~ctrl:(of_int ~width:2 0) ~rst_n:i.rst_n ~rgb_clk:i.rgb_clk ~rgb_de:i.rgb_de in
    let enc_r = encode ~scope ~inst:"red" ~data:i.rgb_r ~ctrl:(of_int ~width:2 0) ~rst_n:i.rst_n ~rgb_clk:i.rgb_clk ~rgb_de:i.rgb_de in

    (* 2. Serializers (OSER10) *)
    let rst = ~: (i.rst_n) in
    let ser_clk = oser10 ~inst:"clk_ser" ~data:(of_int ~width:10 0b1111100000) ~pclk:i.rgb_clk ~fclk:i.serial_clk ~reset:rst in
    let ser_b = oser10 ~inst:"blu_ser" ~data:enc_b ~pclk:i.rgb_clk ~fclk:i.serial_clk ~reset:rst in
    let ser_g = oser10 ~inst:"grn_ser" ~data:enc_g ~pclk:i.rgb_clk ~fclk:i.serial_clk ~reset:rst in
    let ser_r = oser10 ~inst:"red_ser" ~data:enc_r ~pclk:i.rgb_clk ~fclk:i.serial_clk ~reset:rst in

    (* 3. LVDS Output Buffers *)
    let clk_p, clk_n = elvds_obuf ~inst:"clk_obuf" ~input:ser_clk in
    let b_p, b_n     = elvds_obuf ~inst:"blu_obuf" ~input:ser_b in
    let g_p, g_n     = elvds_obuf ~inst:"grn_obuf" ~input:ser_g in
    let r_p, r_n     = elvds_obuf ~inst:"red_obuf" ~input:ser_r in

    { O.
      tmds_clk_p  = clk_p;
      tmds_clk_n  = clk_n;
      tmds_data_p = concat_msb [r_p; g_p; b_p];
      tmds_data_n = concat_msb [r_n; g_n; b_n];
    }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"dvi_tx" ~instance:"inst1" create i
end
