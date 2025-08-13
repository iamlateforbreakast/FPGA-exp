(* counter.ml *)

open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t =
    { clk : 'a }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { leds : 'a[@bits 6]
    }
  [@@deriving hardcaml]
end

(* Frequency is 27MHz to 1/2 s wait time betwen led increment *)
let wait_time = 13_499_999

let create (i : _ I.t) =
  let clk_counter = reg_fb
                       (Reg_spec.create ~clock:i.clk ~clear:gnd ())
                       ~enable:vdd
                       ~width:32
                       ~f:(fun d -> 
                        mux2 (d ==:. wait_time) (zero 32) (d +:. 1)) in
  let led_counter = reg_fb
                       (Reg_spec.create ~clock:i.clk ~clear:gnd ())
                       ~enable:vdd
                       ~width: 6
                       ~f:(fun d -> 
                        mux2 (clk_counter ==:. wait_time) (d +:. 1) d) in
  { O.leds = ~:led_counter 
  }
 