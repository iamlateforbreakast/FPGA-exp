open Hardcaml
(** A configuration for the screen module. *)
module type Config = Config_intf.S

(** The top-level screen module. *)
module Make ( _ : Config) : sig
  (** The input interface. *)
  module I : sig
    type 'a t =
      { i_clk : 'a [@bits 1]  (** System clock input *)
      ; i_reset : 'a [@bits 1]  (** System reset input *)
      }
    [@@deriving hardcaml]
  end

  (** The output interface. *)
  module O : sig
    type 'a t =
      { o_sclk  : 'a [@bits 1]  (** SPI clock output *)
      ; o_sdin  : 'a [@bits 1]  (** SPI data output *)
      ; o_cs    : 'a [@bits 1]  (** Chip select output *)
      ; o_dc    : 'a [@bits 1]  (** Data/command select output *)
      ; o_reset : 'a [@bits 1]  (** Reset output *)
      }
    [@@deriving hardcaml]
  end

  (** Creates the hardware circuit for the screen module.

      This function takes a [scope] and the input signals [i] and returns the
      output signals. *)
  val create
    :  Scope.t
    -> Signal.t I.t
    -> Signal.t O.t

  (** Creates a hierarchical hardware module for the screen.

      This function is a convenience wrapper around [create] that registers
      the circuit within a parent scope. *)
  val hierarchical
    :  Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
