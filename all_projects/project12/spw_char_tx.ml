(** Spw_char_tx — SpaceWire character serialiser (TX side).

    Accepts 10-bit SpaceWire characters from the link FSM and shifts them
    out LSB-first to the DS encoder at the TX bit rate.

    SpaceWire character encoding (ECSS §8.5):
      For a *data* character  : [parity(9)] [flag=0(8)] [D7..D0]
      For a *control* character: [parity(9)] [flag=1(8)] [C3..C0 0000]

    Parity is odd parity over bits [8:0].

    NULL  = ESC + FCT  (two characters back-to-back)
    FCT   = flag=1, C=001
    EOP   = flag=1, C=010
    EEP   = flag=1, C=100
    ESC   = flag=1, C=000  (never sent alone)

    Ports
    ─────
    clock     : TX bit clock
    clear     : synchronous reset
    char_valid : upstream has a character ready
    char_data  : 9-bit payload [flag, D7..D0 or C3..C0,0,0,0,0]
    char_ready : asserted when this module can accept a new character
    d_enc_valid: output to DS encoder valid
    d_enc_bit  : output bit to DS encoder *)

open Hardcaml
open Signal
open Spw_constants

(* ── Character helpers ──────────────────────────────────────────────────── *)

(** Compute odd parity over 9 bits so that the total number of 1s in the
    10-bit character (including parity) is odd. *)
let odd_parity9 (v : Signal.t) : Signal.t =
  (* XOR all 9 bits; that gives even parity; invert for odd. *)
  let xors =
    List.init 9 (bit v)
    |> List.fold_left ( ^: ) gnd
  in
  ~: xors

module I = struct
  type 'a t =
    { clock      : 'a [@bits 1]
    ; clear      : 'a [@bits 1]
    ; char_valid : 'a [@bits 1]  (** character is available         *)
    ; char_data  : 'a [@bits 9]  (** [8]=flag, [7:0]=data/control   *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { char_ready  : 'a [@bits 1]  (** ready to accept next character *)
    ; enc_valid   : 'a [@bits 1]  (** one bit ready for DS encoder   *)
    ; enc_bit     : 'a [@bits 1]  (** the bit to encode              *)
    }
  [@@deriving hardcaml]
end

let create (i : _ I.t) : _ O.t =
  let spec = reg_spec ~clock:i.clock ~clear:i.clear () in

  (* Build the full 10-bit character with parity *)
  let parity   = odd_parity9 i.char_data -- "tx_parity" in
  let char10   = concat_msb [parity; i.char_data] -- "char10" in

  (* Shift register: 10 bits wide, loaded on char_valid when idle *)
  let shift_reg = wire spw_char_bits -- "shift_reg" in
  let bit_cnt   = wire (num_bits_to_represent spw_char_bits) -- "bit_cnt" in

  let idle   = bit_cnt ==: zero (num_bits_to_represent spw_char_bits) -- "idle" in
  let load   = idle &: i.char_valid -- "load" in

  let shift_next =
    mux2 load
      char10                                            (* load new char  *)
      (concat_msb [gnd; sel_top shift_reg (spw_char_bits - 1)]) (* shift right *)
    -- "shift_next"
  in
  let shift_reg_r = reg spec ~enable:(load |: (~: idle)) shift_next in
  shift_reg <== shift_reg_r;

  let cnt_next =
    mux2 load
      (of_int ~width:(num_bits_to_represent spw_char_bits) spw_char_bits)
      (bit_cnt -: one (num_bits_to_represent spw_char_bits))
    -- "cnt_next"
  in
  let bit_cnt_r = reg spec ~enable:vdd cnt_next in
  bit_cnt <== bit_cnt_r;

  { O.
    char_ready = idle
  ; enc_valid  = ~: idle
  ; enc_bit    = lsb shift_reg_r   (* LSB first per SpaceWire spec *)
  }

let circuit () =
  let module C = Circuit.With_interface (I) (O) in
  C.create_exn ~name:"spw_char_tx" create
