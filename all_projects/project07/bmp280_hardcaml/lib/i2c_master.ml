(* i2c_master.ml *)
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
      { sclk : 'a
      ; i2c 'a
      }
    [@@deriving hardcaml]
  end
	
  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
	  let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in
    let counter_1s = reg_fb sync_spec 
                       ~enable:vdd 
                       ~width:32 
                       ~f:(fun d -> mux2 (d ==:. wait_time) (zero 32) (d +:. 1)) in

    (* Return circuit output value *)
    { O.i2c_master = i2c_master.data }  (* Placeholder for actual LED output *)

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"i2c_master" ~instance:"inst1" create i
    
end
