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
end
