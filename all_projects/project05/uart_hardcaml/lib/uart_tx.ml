(* uart_tx.ml *)

open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a
      ; resetn : 'a
      ; data : 'a [@bits 8]
      ; data_valid : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { data : 'a
      ; data_ready : 'a
      }
    [@@deriving hardcaml]
  end

  (* State encoding *)
  module States = struct
    type t = S_IDLE | S_START | S_SEND_BYTE | S_STOP
    [@@deriving sexp_of, compare, enumerate]
  end

  let create (_scope : Scope.t) (_input : Signal.t I.t) : Signal.t O.t =
    {O.data = vdd; data_ready = vdd}
  
  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"uart_tx" ~instance:"inst1" create i
end
