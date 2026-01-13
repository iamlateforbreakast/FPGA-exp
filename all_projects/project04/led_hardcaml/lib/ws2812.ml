(* ws2812.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
  type 'a t = {
    clock   : 'a;
    reset : 'a;
    color : 'a; [@bits 24]
  } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      data : 'a;
    } [@@deriving hardcaml]
  end

  module State = struct
    type t = IDLE | DATA_SEND | BIT_SEND_HIGH | BIT_SEND_LOW
    [@@deriving enumerate, compare]
  end
  
  let create (_scope: Scope.t) (input: Signal.t I.t) : Signal.t O.t =
    let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in
    let data_reg = Always.Variable.reg sync_spec ~width:1 in
    { O.data = data_reg.value }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"ws2812" ~instance:"inst1" create i

end
