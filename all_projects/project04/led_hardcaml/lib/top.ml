(* top.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds : 'a[@bits 6]
      }
    [@@deriving hardcaml]
  end

  let color_rom ~index =
    let open Signal in
    let rom = List.map (fun c -> of_int ~width:24 c) X.colors in
    mux index rom
	
  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let open Always in
    let wait_time = 26_999_999 in
    let ws2812 = Ws2812.hierarchical scope (
	     Ws2812.I.{ resetn=input.resetn; clock=input.clock }) in
    let sync_spec = Reg_spec.create ~clock:input.clk ~reset:input.reset () in
    let counter_1s = reg_fb sync_spec 
                       ~enable:vdd 
                       ~width:32 
                       ~f:(fun d -> mux2 (d ==:. wait_time) (zero 32) (d +:. 1)) in
    let color_index = reg_fb sync_spec
                        ~enable: vdd
                        ~width:6
                        ~f:(fun d -> mux2 (counter_1s ==:. wait_time)(mux2 (d ==:. 2) (zero 6) (d +:. 1)) d) in

    (* Return circuit output value *)
    { O.leds = color_index.value }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
    
end
