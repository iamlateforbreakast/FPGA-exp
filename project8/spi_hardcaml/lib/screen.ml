(* screen.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clk : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { io_sclk : 'a
      ; io_sdin : 'a
      ; io_cs : 'a
      ; io_dc : 'a
      ; io_reset : 'a
      }
    [@@deriving hardcaml]
  end
end
