open Hardcaml

module I = struct
  type 'a t =
    { clkin : 'a
    } 
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { lock : 'a
    ; clkout : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let create (input : _ I.t) =
  let module Inst = Instantiation.With_interface(I)(O) in
  Inst.create ~name:"gowin_rpll" input
;;
