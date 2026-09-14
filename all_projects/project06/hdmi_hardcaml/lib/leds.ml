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
      { leds : 'a[@bits X.led_width]
      } [@@deriving hardcaml]
  end

  let pattern_rom ~index =
    let open Signal in
    let rom = List.map (fun c -> of_int ~width:6 c) X.pattern in
    mux index rom
  
  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let clk_counter = reg_fb
                         (Reg_spec.create ~clock:input.clock ~clear:gnd ())
                         ~enable:vdd
                         ~width:32
                         ~f:(fun d -> 
                          mux2 (d ==:. X.clk_fre) (zero 32) (d +:. 1)) in
    let led_counter = reg_fb
                       (Reg_spec.create ~clock:input.clock ~clear:gnd ())
                       ~enable:vdd
                       ~width: 6
                       ~f:(fun d -> 
                        mux2 (clk_counter ==:. X.clk_fre) 
                        (mux2 (d ==:. (List.length X.pattern - 1))(zero 6)(d +:. 1)) d) in
    let _ = Signal.(clk_counter -- "dbg_clk_counter") in
    let _ = Signal.(led_counter -- "dbg_led_counter") in
    (* On boards with fewer physical LEDs than the pattern's 6-bit mask
       width (e.g. the Nano 4K's single LED), only the low X.led_width bits
       of each pattern step are driven. *)
    { O.leds = select (pattern_rom ~index:led_counter) (X.led_width - 1) 0
    }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"leds" ~instance:"inst1" create i
end
 
