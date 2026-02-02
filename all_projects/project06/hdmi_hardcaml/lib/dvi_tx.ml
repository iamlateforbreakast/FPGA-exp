(* dvi_tx.ml *)
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

  let create (i : Signal.t I.t) =
  (* Helper for OSER10 instantiation *)
  let oser10 ~name ~data ~pclk ~fclk ~reset =
    Instantiation.create 
      ~name:"OSER10"
      ~parameters:[
        "GSREN", Parameter.string "false";
        "LSREN", Parameter.string "true"
      ]
      ~inputs:(
        List.init 10 ~f:(fun bit -> (Printf.sprintf "D%d" bit, bit_ data bit)) @
        [ "PCLK", pclk; "FCLK", fclk; "RESET", reset ]
      )
      ~outputs:[ "Q", 1 ]
      ()
    |> fun m -> Map.find_exn m "Q"
  in

  (* Helper for ELVDS_OBUF instantiation *)
  let elvds_obuf ~name ~input =
    let m = Instantiation.create
      ~name:"ELVDS_OBUF"
      ~inputs:[ "I", input ]
      ~outputs:[ "O", 1; "OB", 1 ]
      ()
    in
    (Map.find_exn m "O", Map.find_exn m "OB")
  in

  (* 1. DVI Encoders (Assumes an external dvi_encoder module exists) *)
  let encode ~name ~data ~ctrl =
    Instantiation.create 
      ~name:"dvi_encoder"
      ~inputs:[
        "I_rst_n", i.rst_n;
        "I_clk", i.rgb_clk;
        "I_de", i.rgb_de;
        "I_data", data;
        "I_ctrl", ctrl;
      ]
      ~outputs:[ "O_data", 10 ]
      ()
    |> fun m -> Map.find_exn m "O_data"
  in

  let enc_b = encode ~name:"blu" ~data:i.rgb_b ~ctrl:(concat_msb [i.rgb_vs; i.rgb_hs]) in
  let enc_g = encode ~name:"grn" ~data:i.rgb_g ~ctrl:(of_int ~width:2 0) in
  let enc_r = encode ~name:"red" ~data:i.rgb_r ~ctrl:(of_int ~width:2 0) in

  (* 2. Serializers (OSER10) *)
  let rst = ~: (i.rst_n) in
  let ser_clk = oser10 ~name:"clk_ser" ~data:(of_int ~width:10 0b1111100000) ~pclk:i.rgb_clk ~fclk:i.serial_clk ~reset:rst in
  let ser_b = oser10 ~name:"blu_ser" ~data:enc_b ~pclk:i.rgb_clk ~fclk:i.serial_clk ~reset:rst in
  let ser_g = oser10 ~name:"grn_ser" ~data:enc_g ~pclk:i.rgb_clk ~fclk:i.serial_clk ~reset:rst in
  let ser_r = oser10 ~name:"red_ser" ~data:enc_r ~pclk:i.rgb_clk ~fclk:i.serial_clk ~reset:rst in

  (* 3. LVDS Output Buffers *)
  let clk_p, clk_n = elvds_obuf ~name:"clk_obuf" ~input:ser_clk in
  let b_p, b_n     = elvds_obuf ~name:"blu_obuf" ~input:ser_b in
  let g_p, g_n     = elvds_obuf ~name:"grn_obuf" ~input:ser_g in
  let r_p, r_n     = elvds_obuf ~name:"red_obuf" ~input:ser_r in

  { O.
    tmds_clk_p  = clk_p;
    tmds_clk_n  = clk_n;
    tmds_data_p = concat_msb [r_p; g_p; b_p];
    tmds_data_n = concat_msb [r_n; g_n; b_n];
  }
end
