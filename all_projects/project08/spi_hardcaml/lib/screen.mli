open Hardcaml
(** A configuration for the screen module. *)
module type Config = Config.S

(** The top-level screen module. *)
module Make ( _ : Config) : sig
  (** The input interface. *)
  module I : sig
    type 'a t =
      { clock : 'a [@bits 1]  (** System clock input *) 
      ; i_reset : 'a [@bits 1]  (** System reset input *)
      }
    [@@deriving hardcaml]
  end

  (** The output interface. *)
  module O : sig
    type 'a t =
      { io_sclk  : 'a [@bits 1]  (** SPI clock output *)
      ; io_sdin  : 'a [@bits 1]  (** SPI data output *)
      ; io_cs    : 'a [@bits 1]  (** Chip select output *)
      ; io_dc    : 'a [@bits 1]  (** Data/command select output *)
      ; io_reset : 'a [@bits 1]  (** Reset output *)
      ; debug1 : 'a [@bits 4]  (* For debugging purposes *)
      ; debug2 : 'a [@bits 10]  (* For debugging purposes *)
      ; debug3 : 'a [@bits 8]  (* For debugging purposes *)
      ; debug4: 'a [@bits 4]
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
end
