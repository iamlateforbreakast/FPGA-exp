(* counter.ml *)

open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clk : 'a;
      clear : 'a;
      incr  : 'a }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { leds : 'a[@bits 6]
    }
  [@@deriving hardcaml]
end

let create (i : _ I.t) =
  { O.leds = reg_fb
      (Reg_spec.create ~clock:i.clk ~clear:i.clear ())
      ~enable:i.incr
      ~width:6
      ~f:(fun d -> d +:. 1)
  }
