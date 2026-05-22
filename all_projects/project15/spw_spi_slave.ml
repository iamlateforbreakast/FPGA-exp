(** Spw_spi_slave — SPI mode-0 slave (CPOL=0, CPHA=0), 16-bit frames.

    Frame format (MSB transmitted first):
      Bit 15   : R/nW  — 1 = read, 0 = write
      Bits 14:8: ADDR  — 7-bit register address
      Bits 7:0 : DATA  — write payload (ignored on reads)

    On a read, the MISO line is loaded with the register value at the
    falling edge of CS (before SCK begins), so the host can shift it
    out MSB-first during the frame.

    This module operates entirely in the SPI clock domain and produces
    registered outputs.  The register file connects directly.

    Ports
    ─────
    clock    : system clock (for double-flop synchronisers on SPI pins)
    clear    : synchronous reset
    sck      : SPI clock from host
    cs_n     : Chip-select (active low)
    mosi     : Master-out slave-in
    miso     : Master-in slave-out
    reg_addr : decoded register address (valid when wr_en or rd_en)
    reg_wdata: write data
    wr_en    : single-cycle write strobe (in sys-clock domain)
    rd_en    : single-cycle read  strobe
    reg_rdata: read data presented to MISO shift register *)

open Hardcaml
open Signal
open Hardcaml.Always
open Spw_constants

module I = struct
  type 'a t =
    { clock     : 'a [@bits 1]
    ; clear     : 'a [@bits 1]
    (* SPI pins — raw from I/O pads, will be synchronised inside *)
    ; sck       : 'a [@bits 1]
    ; cs_n      : 'a [@bits 1]
    ; mosi      : 'a [@bits 1]
    (* Read-data from register file, must be valid cycle after rd_en *)
    ; reg_rdata : 'a [@bits spi_data_bits]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { miso     : 'a [@bits 1]
    ; reg_addr : 'a [@bits spi_addr_bits]
    ; reg_wdata: 'a [@bits spi_data_bits]
    ; wr_en    : 'a [@bits 1]
    ; rd_en    : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

let create (i : _ I.t) : _ O.t =
  let spec = reg_spec ~clock:i.clock ~clear:i.clear () in

  (* ── Double-flop synchronisers for async SPI inputs ── *)
  let sync s =
    let s0 = reg spec ~enable:vdd s in
    reg spec ~enable:vdd s0
  in
  let sck_s  = sync i.sck  -- "sck_s"  in
  let cs_s   = sync i.cs_n -- "cs_s"   in
  let mosi_s = sync i.mosi -- "mosi_s" in

  (* Edge detectors for SCK in sys-clock domain *)
  let sck_prev  = reg spec ~enable:vdd sck_s  -- "sck_prev"  in
  let sck_rise  = sck_s  &: (~: sck_prev) -- "sck_rise"  in
  let cs_prev   = reg spec ~enable:vdd cs_s   -- "cs_prev"   in
  let cs_fall   = (~: cs_s) &: cs_prev    -- "cs_fall"   in  (* CS went low *)
  let cs_rise   = cs_s  &: (~: cs_prev)   -- "cs_rise"   in  (* CS went high = end of frame *)

  let active = ~: cs_s -- "active" in

  (* ── Shift register: 16 bits, MSB first ── *)
  let sr       = Always.Variable.reg ~width:spi_frame_bits spec ~enable:vdd in
  let bit_cnt  = Always.Variable.reg
                   ~width:(num_bits_to_represent spi_frame_bits) spec ~enable:vdd in

  (* ── MISO shift register: loaded on CS fall with reg_rdata ── *)
  (* We pre-load the read register so data is valid from bit 0 onward.
     The address isn't known until bit 7 arrives, so for reads the host
     must perform a dummy first transaction or accept a 1-frame latency.
     A simpler scheme: latch the *previous* read result.  The register
     file holds its output until the next rd_en. *)
  let miso_sr = Always.Variable.reg ~width:spi_data_bits spec ~enable:vdd in

  (* Decoded fields from the completed frame *)
  let rw_bit   = bit sr.value (spi_frame_bits - 1)              -- "rw_bit"    in
  let addr_f   = sel_top (sel_bottom sr.value (spi_frame_bits - 1)) spi_addr_bits
                 -- "addr_f"  in
  let data_f   = sel_bottom sr.value spi_data_bits               -- "data_f"   in

  let wr_en_w  = Always.Variable.wire ~default:gnd in
  let rd_en_w  = Always.Variable.wire ~default:gnd in

  compile [
    wr_en_w <-- gnd;
    rd_en_w <-- gnd;

    (* On CS falling: reset counter, pre-load MISO with last read result *)
    when_ cs_fall [
      bit_cnt <-- zero (num_bits_to_represent spi_frame_bits);
      miso_sr <-- uresize i.reg_rdata spi_data_bits;
    ];

    (* On each SCK rising edge while CS active: shift in MOSI *)
    when_ (active &: sck_rise) [
      sr      <-- concat_msb [sel_top sr.value (spi_frame_bits - 1); mosi_s];
      miso_sr <-- concat_msb [sel_top miso_sr.value (spi_data_bits - 1); gnd];
      bit_cnt <-- bit_cnt.value +: one (num_bits_to_represent spi_frame_bits);
    ];

    (* On CS rising edge: frame complete — issue strobe *)
    when_ cs_rise [
      when_ rw_bit  [ rd_en_w <-- vdd ];
      when_ (~: rw_bit) [ wr_en_w <-- vdd ];
    ];
  ];

  { O.
    miso      = msb miso_sr.value
  ; reg_addr  = addr_f
  ; reg_wdata = data_f
  ; wr_en     = wr_en_w.value
  ; rd_en     = rd_en_w.value
  }

let circuit () =
  let module C = Circuit.With_interface (I) (O) in
  C.create_exn ~name:"spw_spi_slave" create
