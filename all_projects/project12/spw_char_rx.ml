(** Spw_char_rx — SpaceWire character deserialiser (RX side).

    Collects individual bits from the DS decoder, groups them into 10-bit
    SpaceWire characters (LSB first), checks odd parity, and emits the
    9-bit payload together with a flag indicating data vs control and a
    separate parity_error signal.

    Ports
    ─────
    clock        : system clock (same domain as DS decoder output)
    clear        : synchronous reset
    bit_valid    : one-cycle pulse from DS decoder when a bit is ready
    bit_in       : the recovered bit
    char_valid   : one-cycle pulse when a full character has been decoded
    char_data    : [8]=flag, [7:0]=data or control nibble
    parity_error : asserted alongside char_valid when parity fails *)

open Hardcaml
open Signal
open Spw_constants

module I = struct
  type 'a t =
    { clock      : 'a [@bits 1]
    ; clear      : 'a [@bits 1]
    ; bit_valid  : 'a [@bits 1]
    ; bit_in     : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { char_valid   : 'a [@bits 1]
    ; char_data    : 'a [@bits 9]  (** [8]=flag, [7:0]=payload *)
    ; parity_error : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

let create (i : _ I.t) : _ O.t =
  let spec = reg_spec ~clock:i.clock ~clear:i.clear () in

  (* Shift register accumulates bits LSB-first *)
  let shift_reg = wire spw_char_bits -- "rx_shift" in
  let shift_next =
    mux2 i.bit_valid
      (concat_msb [i.bit_in; sel_top shift_reg (spw_char_bits - 1)])
      shift_reg
    -- "rx_shift_next"
  in
  let shift_r = reg spec ~enable:vdd shift_next in
  shift_reg <== shift_r;

  (* Bit counter 0..9; wraps at 10 *)
  let cnt_w  = num_bits_to_represent (spw_char_bits - 1) in
  let cnt    = wire cnt_w -- "rx_bit_cnt" in
  let cnt_next =
    mux2 i.bit_valid
      (mux2 (cnt ==: of_int ~width:cnt_w (spw_char_bits - 1))
        (zero cnt_w)
        (cnt +: one cnt_w))
      cnt
  in
  let cnt_r = reg spec ~enable:vdd cnt_next in
  cnt <== cnt_r;

  (* char_valid fires on the cycle the 10th bit (parity) is shifted in *)
  let char_valid =
    i.bit_valid &: (cnt ==: of_int ~width:cnt_w (spw_char_bits - 1))
    -- "char_valid"
  in

  (* Parity check: XOR all 10 bits; for odd parity the result must be 1 *)
  let parity_ok =
    List.init spw_char_bits (fun k -> bit shift_r k)
    |> List.fold_left ( ^: ) gnd
    -- "parity_ok"
  in

  { O.
    char_valid   = char_valid
  ; char_data    = sel_bottom shift_r 9    (* bits [8:0] = flag + payload *)
  ; parity_error = char_valid &: (~: parity_ok)
  }

let circuit () =
  let module C = Circuit.With_interface (I) (O) in
  C.create_exn ~name:"spw_char_rx" create
