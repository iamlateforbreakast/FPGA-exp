(** A configuration for the screen module. *)
module type Config = Config_intf.S

(** The top-level screen module. *)
module Make (X : Config) : sig
  (** The input interface. *)
  module I : Hardcaml.With_interface.S

  (** The output interface. *)
  module O : Hardcaml.With_interface.S

  (** Creates the hardware circuit for the screen module.

      This function takes a [scope] and the input signals [i] and returns the
      output signals. *)
  val create
    :  Hardcaml.Scope.t
    -> 'a I.t
    -> 'a O.t

  (** Creates a hierarchical hardware module for the screen.

      This function is a convenience wrapper around [create] that registers
      the circuit within a parent scope. *)
  val hierarchical
    :  Hardcaml.Scope.t
    -> Hardcaml.Signal.t I.t
    -> Hardcaml.Signal.t O.t
end
