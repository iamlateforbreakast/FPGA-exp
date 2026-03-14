(** Spw_ds_decoder — SpaceWire Data-Strobe decoder with 4× oversampling (RX side).

    The receiver samples both D and S at 4× the nominal bit rate using the
    system clock.  A bit boundary is detected whenever (D XOR S) changes
    value — at that point the majority-voted sample in the *previous* window
    is a valid data bit.

    Oversampling avoids the need for a CDR PLL on the Gowin GW2AR-18 fabric.
    At 10 Mbit/s the system clock must be ≥ 40 MHz (use the on-chip PLL from
    27 MHz crystal → 54 MHz works well).

    Ports
    ─────
    clock     : system clock (≥ 4 × bit rate)
    clear     : synchronous active-high reset
    d_in      : Data   line from TLVDS_IBUF
    s_in      : Strobe line from TLVDS_IBUF
    bit_valid : single-cycle pulse when a new bit has been decoded
    bit_out   : the recovered data bit
    error     : parity or disconnect error indicator *)

open Hardcaml
open Signal
open Spw_constants

module I = struct
  type 'a t =
    { clock  : 'a [@bits 1]
    ; clear  : 'a [@bits 1]
    ; d_in   : 'a [@bits 1]  (** raw pin — sampled by this module *)
    ; s_in   : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { bit_valid : 'a [@bits 1]  (** pulse: recovered bit is ready  *)
    ; bit_out   : 'a [@bits 1]  (** the recovered data bit         *)
    ; transition : 'a [@bits 1] (** pulse on every DS transition   *)
    }
  [@@deriving hardcaml]
end

(* Number of system clock ticks per bit period at 4× oversampling.
   At runtime the link_cfg.tx_div register sets the actual rate;
   here we build the logic for a fixed 4-sample window. *)
let os_ratio = 4  (* oversampling ratio — adjust for higher speeds *)

let create (i : _ I.t) : _ O.t =
  let spec = reg_spec ~clock:i.clock ~clear:i.clear () in

  (* ── Double-flop synchroniser for the two async input pins ── *)
  let sync ff =
    let s0 = reg spec ~enable:vdd ff in
    reg spec ~enable:vdd s0
  in
  let d_sync = sync i.d_in -- "d_sync" in
  let s_sync = sync i.s_in -- "s_sync" in

  (* ── XOR gives the combined transition signal ── *)
  let ds_xor      = d_sync ^: s_sync           -- "ds_xor"      in
  let ds_xor_prev = reg spec ~enable:vdd ds_xor -- "ds_xor_prev" in
  let transition  = ds_xor ^: ds_xor_prev       -- "transition"  in

  (* ── Sample shift register: keep last os_ratio samples of D ── *)
  (* We collect data bits between transitions. When a transition fires
     we emit the majority vote of the collected window. *)
  let sample_sr = reg spec ~enable:vdd
    (concat_msb [ sel_bottom (reg spec ~enable:vdd
                    (concat_msb
                      [ sel_bottom (reg spec ~enable:vdd
                          (concat_msb [d_sync; zero (os_ratio - 1)]))
                          (os_ratio - 1)
                      ; d_sync ]))
                    (os_ratio - 1)
                ; d_sync ])
    -- "sample_sr"
  in
  (* Majority vote across os_ratio samples (simple popcount ≥ os_ratio/2) *)
  let popcount =
    List.init os_ratio (fun k -> bit sample_sr k)
    |> List.fold_left ( +: ) (zero (num_bits_to_represent os_ratio))
  in
  let majority = uresize popcount 1 -- "majority" in

  (* ── Output ── *)
  { O.
    bit_valid  = transition   (* one valid bit per DS transition *)
  ; bit_out    = majority
  ; transition
  }

let circuit () =
  let module C = Circuit.With_interface (I) (O) in
  C.create_exn ~name:"spw_ds_decoder" create
