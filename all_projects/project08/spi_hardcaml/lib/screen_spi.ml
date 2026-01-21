(* screen_spi.ml *)
open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

module Make (X : Config.S) = struct
  module I = struct
    type 'a t =
      { clk   : 'a
      ; clear : 'a
      ; data_in : 'a [@bits 8]
      } 
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { mosi : 'a
      ; sclk : 'a
      ; busy : 'a
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
  { O.mosi = zero 1; sclk = zero 1; busy = zero 1; done_ = zero 1 }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"screen_spi" ~instance:"inst1" create i
end
