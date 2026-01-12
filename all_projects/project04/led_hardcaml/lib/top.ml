(* top.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a
      ; resetn : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds : 'a[@bits 6]
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let open Always in
    let ws2812 = Ws2812.hierarchical scope (
	     Ws2812.I.{ resetn=i.resetn; clock=i.clock }) in
    let sync_spec = Reg_spec.create ~clock:input.clk ~reset:input.reset () in
    let colors = [ 0xFF0000, 0x00FF00, 0x0000FF ] in
    let counter_1s = reg_fb sync_spec
                       ~enable:vdd
                       ~width:32
                       ~f:(fun d -> mux2 (d ==:. wait_time) (zero 32) (d +:. 1)) in
    let color_index = reg_fb sync_spec
                        ~enable: vdd
                        ~width:6 in
                        ~f:(fun d -> mux2 (counter_1s ==:. 0)(mux 2 (d ==:. 2)(zero 6)(d +:. 1)) d) in

    (* Return circuit output value *)
    { O.leds = color_index.value }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
    
end
