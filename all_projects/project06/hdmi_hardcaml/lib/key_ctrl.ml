(* key_ctrl.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t = {
      rst_n        : 'a;
      clk          : 'a;
      key_in       : 'a; [@bits 4]
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      key_out      : 'a; [@bits 4]
    } [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) (input : _ I.t) =
    let reg_spec = Reg_spec.create ~clock:input.clk ~reset:input.rst_n () in

    let key_reg = Always.Variable.reg reg_spec ~width:4 in

    Always.(
      compile
        [
          key_reg <-- input.key_in;
        ]
    );

    { O.key_out = key_reg }
end