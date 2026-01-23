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
      ; i2c : 'a
      }
    [@@deriving hardcaml]
  end

  module MyI2c_master = I2c_master.Make(X)
	
  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
	  let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in
    let counter_1s = reg_fb sync_spec 
                       ~enable:vdd 
                       ~width:32 
                       ~f:(fun d -> mux2 (d ==:. wait_time) (zero 32) (d +:. 1)) in

    let i2c_master = MyI2c_master.hierarchical scope (
	     MyI2c_masterI.{ reset=input.reset; clock=input.clock }) in
    (* Return circuit output value *)
    { O.leds = zero 6; O.i2c_master = i2c_master.data }  (* Placeholder for actual LED output *)

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
    
end
