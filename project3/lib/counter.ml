open Hardcaml

module I = struct
  type 'a t = { clock : 'a } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { leds : 'a [@bits 6] } [@@deriving sexp_of, hardcaml]
end

let create (_i : _ I.t) = { O.leds = Signal.of_string "111111" }