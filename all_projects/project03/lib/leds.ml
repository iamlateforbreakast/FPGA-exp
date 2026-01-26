(* leds.ml *)
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

let wait_time = 13_499_999
let pattern = [ 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 ]
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
  { O.leds = (mux led_counter pattern) 
  }
 
