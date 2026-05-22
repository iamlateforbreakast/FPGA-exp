(** Spw_ds_encoder — SpaceWire Data-Strobe encoder (TX side).

    SpaceWire uses DS encoding (ECSS-E-ST-50-12C §8.4).  The rule is:

        S[n] = S[n-1]  XOR  NOT (D[n] XOR D[n-1])

    i.e. the strobe toggles whenever data does *not* change, so that the
    receiver can always recover a bit clock from D XOR S transitions.

    This module accepts a single serialised bit per [clk_tx] cycle and
    produces the matching (d, s) pair.  The TX serialiser upstream is
    responsible for presenting bits at the correct rate.

    Ports
    ─────
    clock    : TX bit clock (e.g. 10 MHz for 10 Mbit/s)
    clear    : synchronous active-high reset
    valid    : upstream has a bit to send this cycle
    data_in  : the bit to encode
    d_out    : Data line value to drive onto the pin
    s_out    : Strobe line value to drive onto the pin *)

open Hardcaml
open Signal
open Spw_constants

module I = struct
  type 'a t =
    { clock    : 'a [@bits 1]
    ; clear    : 'a [@bits 1]
    ; valid    : 'a [@bits 1]  (** asserted when data_in is meaningful *)
    ; data_in  : 'a [@bits 1]  (** one serialised bit per TX clock     *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { d_out : 'a [@bits 1]  (** Data  line → TLVDS_OBUF              *)
    ; s_out : 'a [@bits 1]  (** Strobe line → TLVDS_OBUF             *)
    }
  [@@deriving hardcaml]
end

let create (i : _ I.t) : _ O.t =
  let spec = reg_spec ~clock:i.clock ~clear:i.clear () in

  (* Register D and S so we can compute the next-state strobe. *)
  let d_reg = reg spec ~enable:i.valid i.data_in in
  let s_reg = wire 1 -- "s_reg" in

  (* DS rule: S toggles when D does not change. *)
  let d_xor  = i.data_in ^: d_reg          in   (* did data change? *)
  let s_next = s_reg ^: (~: d_xor)         in   (* toggle if it didn't *)
  let s_new  = reg spec ~enable:i.valid s_next in
  s_reg <== s_new;

  { O.
    d_out = i.data_in   (* data goes straight to pin; registered by I/O FF *)
  ; s_out = s_new
  }

(* ── Simulation circuit (for waveterm) ──────────────────────────────────── *)
let circuit () =
  let module C = Circuit.With_interface (I) (O) in
  C.create_exn ~name:"spw_ds_encoder" create
