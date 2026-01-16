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
      { tx_pin : 'a
      ; tx_valid : 'a
      }
    [@@deriving hardcaml]
  end

  let message_rom ~index =
    let open Signal in
    let rom = X.message |> String.to_seq |> List.of_seq
    mux index rom

  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let wait_time  = if (X.is_simulation = false) then 1
	                   else (X.cycle_delay / X.clk_fre) - 1 in
    { O.tx_pin = vdd; tx_valid = vdd }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
end
