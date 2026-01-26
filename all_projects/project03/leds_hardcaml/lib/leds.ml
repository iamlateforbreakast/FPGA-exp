(* leds.ml *)
open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

module Make (X : Config) = struct

  module I = struct
    type 'a t =
      { clock : 'a 
      ; reset : 'a
      } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds : 'a[@bits 6]
      } [@@deriving hardcaml]
  end

  let pattern_rom ~index =
    let open Signal in
    let rom = List.map (fun c -> of_int ~width:6 c) X.pattern in
    mux index rom

  let wait_time = 13_499_999
  
  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let clk_counter = reg_fb
                         (Reg_spec.create ~clock:input.clock ~clear:gnd ())
                         ~enable:vdd
                         ~width:32
                         ~f:(fun d -> 
                          mux2 (d ==:. wait_time) (zero 32) (d +:. 1)) in
    let led_counter = reg_fb
                       (Reg_spec.create ~clock:input.clock ~clear:gnd ())
                       ~enable:vdd
                       ~width: 6
                       ~f:(fun d -> 
                        mux2 (clk_counter ==:. wait_time) (d +:. 1) d) in
    { O.leds = pattern_rom ~index:led_counter
    }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"leds" ~instance:"inst1" create i
end
 
