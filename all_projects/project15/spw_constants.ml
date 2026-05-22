(** Hardcaml_spw — shared constants, register map, and utility types.

    All modules open this file so addresses and widths stay in one place.
    Changing a constant here propagates automatically to the register file,
    SPI slave, and software driver header. *)

open Hardcaml

(* ───────────────────────────── SPI frame geometry ──────────────────────── *)

(** Total SPI frame width in bits.
    Frame layout (MSB first):
      [15]    R/nW   — 1 = read, 0 = write
      [14:8]  ADDR   — 7-bit register address (128 regs)
      [7:0]   DATA   — read-back or write payload *)
let spi_frame_bits = 16

(** Number of address bits inside one SPI frame. *)
let spi_addr_bits = 7

(** Data payload width. *)
let spi_data_bits = 8

(* ─────────────────────────────── Register map ───────────────────────────── *)

(** Every address is 7 bits wide (0x00 … 0x7F).
    Registers marked [RO] ignore writes; [WO] return 0x00 on reads;
    [W1C] bits clear when the host writes a 1 to them. *)
module Reg = struct
  (** [0x00] Control register.
      Bit 0 : link_en      — assert to allow the link FSM to leave ErrorReset.
      Bit 1 : loopback     — connect TX DS output directly to RX DS input.
      Bit 2 : tx_en        — enable the TX serialiser (must be set after link_en).
      Bit 3 : soft_reset   — W1C, drives a single-cycle reset into the link FSM. *)
  let ctrl         = 0x00

  (** [0x01] Status register [RO].
      Bits 2:0 : link_state  — mirrors the 3-bit FSM state encoding.
      Bit  3   : rx_avail    — RX FIFO is non-empty.
      Bit  4   : tx_ready    — TX FIFO has space.
      Bit  5   : disconnect  — set when a disconnect error is detected.
      Bit  6   : parity_err  — set on RX parity mismatch.
      Bit  7   : credit_err  — set on credit-count underflow. *)
  let status       = 0x01

  (** [0x02] TX data register [WO].
      Write an 8-bit data character into the TX FIFO.
      Writes are silently dropped when the FIFO is full. *)
  let tx_data      = 0x02

  (** [0x03] TX control register [WO].
      Bit 0 : send_eop      — enqueue a normal End-of-Packet marker.
      Bit 1 : send_eep      — enqueue an Error End-of-Packet marker.
      Bit 2 : send_timecode — transmit the value currently in TIMECODE reg. *)
  let tx_ctrl      = 0x03

  (** [0x04] RX data register [RO].
      Reads one byte from the head of the RX FIFO.
      Returns 0x00 when the FIFO is empty (check rx_avail first). *)
  let rx_data      = 0x04

  (** [0x05] RX character-type register [RO].
      Latched alongside the last byte popped from the RX FIFO.
      Bit 0 : is_data   — character was a data byte.
      Bit 1 : is_eop    — character was an EOP marker.
      Bit 2 : is_eep    — character was an EEP marker.
      Bit 3 : is_timecode — character was a time-code. *)
  let rx_ctrl      = 0x05

  (** [0x06] Timecode register [R/W].
      Bits 5:0 : timecode value (6-bit counter per ECSS spec).
      Bits 7:6 : time-code flags field. *)
  let timecode     = 0x06

  (** [0x07] IRQ enable mask [R/W].
      Each bit enables the corresponding interrupt source.
      Bit mapping mirrors IRQ_STATUS below. *)
  let irq_en       = 0x07

  (** [0x08] IRQ status register [R/W1C].
      Bit 0 : rx_avail     — RX FIFO became non-empty.
      Bit 1 : tx_empty     — TX FIFO drained to zero.
      Bit 2 : link_up      — link FSM entered Run state.
      Bit 3 : link_down    — link FSM left Run state.
      Bit 4 : parity_err   — parity error on received character.
      Bit 5 : disconnect   — disconnect timeout fired.
      Bit 6 : credit_err   — credit counter error. *)
  let irq_status   = 0x08

  (** [0x09] Link configuration register [R/W].
      Bits 3:0 : tx_div    — TX bit-clock divider (PLL output ÷ (tx_div+1)).
      Bits 5:4 : rx_os     — RX oversampling ratio (00=×4, 01=×8, 10=×16).
      Bit  6   : auto_start — re-start link automatically after disconnect. *)
  let link_cfg     = 0x09

  (** Total number of addressable registers (must fit in spi_addr_bits). *)
  let count        = 0x10
end

(* ──────────────────────────── SpaceWire constants ───────────────────────── *)

(** SpaceWire character width at the exchange layer (8 data + 1 parity + 1 flag). *)
let spw_char_bits = 10

(** Depth (in entries) of the TX and RX FIFOs. Must be a power of two. *)
let fifo_depth = 64

(** Bit-width of FIFO address pointers.  log2(fifo_depth). *)
let fifo_addr_bits = 6

(** SpaceWire link-state encoding (3 bits). *)
module Link_state = struct
  let error_reset  = 0  (* Driving reset; sending NULLs              *)
  let error_wait   = 1  (* Waiting 6.4 µs before attempting recovery *)
  let ready        = 2  (* Sending NULLs; waiting for gotNULL        *)
  let started      = 3  (* Sending NULLs+FCTs; waiting for gotFCT   *)
  let connecting   = 4  (* Sending data; waiting for first FCT       *)
  let run          = 5  (* Normal operation                          *)
  let bits         = 3
end

(* ──────────────────────────────── Utilities ─────────────────────────────── *)

(** [reg_spec ~clock ~clear ()] wraps [Reg_spec.create] for convenience. *)
let reg_spec ~clock ~clear () = Reg_spec.create ~clock ~clear ()

(** [rising_edge spec sig] produces a one-cycle pulse on the rising edge of [sig]. *)
let rising_edge spec s =
  let open Signal in
  let s_prev = reg spec ~enable:vdd s in
  s &: (~: s_prev)
