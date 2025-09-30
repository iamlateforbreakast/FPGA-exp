(* top.ml *)
open hardcaml

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
    type 'a t =
      { clock :    'a [@bits 1]  (* Need to be called clock for simulation *)
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_tmds_clk_p :  'a [@bits 1]
      ; o_tmds_clk_n :  'a [@bits 1]
      ; o_tmds_data_p : 'a [@bits 3]
      ; o_tmds_data_n : 'a [@bits 3]
      }
    [@@deriving hardcaml]
  end

end
