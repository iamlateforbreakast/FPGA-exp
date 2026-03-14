(** Spw_top — Top-level integration for the Tang Nano 20K.

    Wires together:
      • SPI slave          (spw_spi_slave)
      • Register file      (spw_regfile)
      • TX async FIFO      (spw_async_fifo)  sys_clk → tx_clk
      • RX async FIFO      (spw_async_fifo)  rx_clk  → sys_clk
      • SpW TX char serialiser (spw_char_tx)
      • DS encoder         (spw_ds_encoder)
      • DS decoder         (spw_ds_decoder)
      • SpW RX char deserialiser (spw_char_rx)
      • Link FSM           (spw_link_fsm)

    Gowin-specific primitives (LVDS I/O and PLL) are instantiated here as
    black boxes.  Replace with actual Gowin IP wizard output for synthesis.

    Pin assignments (add to .cst constraints file):
      IO_LOC  "spw_dp"   target_pin;
      IO_LOC  "spw_dn"   target_pin;
      IO_LOC  "spw_sp"   target_pin;
      IO_LOC  "spw_sn"   target_pin;
      IO_LOC  "spi_sck"  target_pin;
      IO_LOC  "spi_cs_n" target_pin;
      IO_LOC  "spi_mosi" target_pin;
      IO_LOC  "spi_miso" target_pin;
      IO_LOC  "irq_n"    target_pin; *)

open Hardcaml
open Signal
open Spw_constants

module I = struct
  type 'a t =
    { sys_clk   : 'a [@bits 1]  (** 27 MHz crystal                   *)
    ; rst_n     : 'a [@bits 1]  (** active-low async reset (button)   *)
    (* SpaceWire pins — LVDS pairs *)
    ; spw_dp    : 'a [@bits 1]  (** Data+  from TLVDS_IBUF            *)
    ; spw_dn    : 'a [@bits 1]  (** Data−                             *)
    ; spw_sp    : 'a [@bits 1]  (** Strobe+                           *)
    ; spw_sn    : 'a [@bits 1]  (** Strobe−                           *)
    (* SPI slave pins *)
    ; spi_sck   : 'a [@bits 1]
    ; spi_cs_n  : 'a [@bits 1]
    ; spi_mosi  : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { spw_d_p   : 'a [@bits 1]  (** Data+  to TLVDS_OBUF              *)
    ; spw_d_n   : 'a [@bits 1]
    ; spw_s_p   : 'a [@bits 1]  (** Strobe+                           *)
    ; spw_s_n   : 'a [@bits 1]
    ; spi_miso  : 'a [@bits 1]
    ; irq_n     : 'a [@bits 1]  (** Active-low interrupt to host MCU   *)
    ; led_link  : 'a [@bits 1]  (** LED: on when link is in Run state  *)
    }
  [@@deriving hardcaml]
end

(* ── Gowin rPLL black box ──────────────────────────────────────────────
   Generates:
     clk_sys  = 54 MHz  (for ≥4× oversampling at 10 Mbit/s, and SPI)
     clk_tx   = 10 MHz  (SpW TX bit clock)

   In Gowin EDA: instantiate the IP Wizard PLL with these outputs.
   Here we create a black-box placeholder for simulation.              *)
let gowin_pll ~clkin : Signal.t * Signal.t =
  let clk_sys = wire 1 -- "clk_sys_pll" in
  let clk_tx  = wire 1 -- "clk_tx_pll"  in
  ignore clkin;
  (* In real synthesis replace with:
     Instantiation.create ()
       ~name:"rPLL"
       ~parameters:[ "FCLKIN", Param.String "27.0"
                   ; "FBDIV_SEL", Param.Int 1
                   ; "IDIV_SEL",  Param.Int 0
                   ; ... ]
       ~input_ports:[ "CLKIN", clkin ]
       ~output_ports:[ "CLKOUT", [clk_sys]; "CLKOUTP", [clk_tx] ]
     *)
  clk_sys, clk_tx

(* ── LVDS input black box ─────────────────────────────────────────────
   On Tang Nano 20K use Gowin TLVDS_IBUF primitive.
   For simulation / nextpnr: wire directly (single-ended approximation). *)
let lvds_ibuf ~p ~_n : Signal.t =
  (* Synthesis: replace with Instantiation of TLVDS_IBUF *)
  p -- "lvds_in"

let create (i : _ I.t) : _ O.t =
  (* ── Clocks and reset ─────────────────────────────────────────────── *)
  let clk_sys, clk_tx = gowin_pll ~clkin:i.sys_clk in
  let clear = ~: i.rst_n -- "clear" in

  (* ── DS decoder (runs at clk_sys, ≥4× oversample) ─────────────────── *)
  let d_raw = lvds_ibuf ~p:i.spw_dp ~_n:i.spw_dn in
  let s_raw = lvds_ibuf ~p:i.spw_sp ~_n:i.spw_sn in

  let dec = Spw_ds_decoder.create
    Spw_ds_decoder.I.{ clock = clk_sys; clear; d_in = d_raw; s_in = s_raw }
  in

  (* ── RX character deserialiser ──────────────────────────────────────── *)
  let char_rx = Spw_char_rx.create
    Spw_char_rx.I.{ clock = clk_sys; clear
                  ; bit_valid = dec.bit_valid; bit_in = dec.bit_out }
  in

  (* ── TX async FIFO (sys_clk → clk_tx) ────────────────────────────── *)
  let module TxFifo = Spw_async_fifo.Data_fifo in
  let tx_fifo_wr_data = wire 9 -- "tx_fifo_wr_data" in
  let tx_fifo_wr_en   = wire 1 -- "tx_fifo_wr_en"   in
  let tx_fifo_rd_ready = wire 1 -- "tx_fifo_rd_ready" in

  let tx_fifo = TxFifo.create
    TxFifo.I.{ wr_clock = clk_sys; wr_clear = clear
             ; rd_clock = clk_tx;  rd_clear = clear
             ; wr_data  = tx_fifo_wr_data
             ; wr_valid = tx_fifo_wr_en
             ; rd_ready = tx_fifo_rd_ready }
  in

  (* ── TX character serialiser ──────────────────────────────────────── *)
  let char_tx = Spw_char_tx.create
    Spw_char_tx.I.{ clock = clk_tx; clear
                  ; char_valid = tx_fifo.rd_valid
                  ; char_data  = tx_fifo.rd_data }
  in
  tx_fifo_rd_ready <== char_tx.char_ready;

  (* ── DS encoder ───────────────────────────────────────────────────── *)
  let enc = Spw_ds_encoder.create
    Spw_ds_encoder.I.{ clock = clk_tx; clear
                     ; valid = char_tx.enc_valid
                     ; data_in = char_tx.enc_bit }
  in

  (* ── RX async FIFO (clk_sys → clk_sys, same domain here) ─────────── *)
  (* In a real design with a recovered RX clock, change rd_clock to clk_rx *)
  let module RxFifo = Spw_async_fifo.Data_fifo in
  let rx_fifo_rd_ready = wire 1 -- "rx_fifo_rd_ready" in

  let rx_fifo = RxFifo.create
    RxFifo.I.{ wr_clock = clk_sys; wr_clear = clear
             ; rd_clock = clk_sys; rd_clear = clear
             ; wr_data  = char_rx.char_data
             ; wr_valid = char_rx.char_valid
             ; rd_ready = rx_fifo_rd_ready }
  in

  (* ── Link FSM ─────────────────────────────────────────────────────── *)
  let link_en_r  = wire 1 -- "link_en"   in
  let link_fsm = Spw_link_fsm.create
    Spw_link_fsm.I.{ clock = clk_sys; clear
                   ; link_en = link_en_r
                   ; auto_start = gnd
                   ; rx_char_valid = char_rx.char_valid
                   ; rx_char_data  = char_rx.char_data
                   ; rx_parity_err = char_rx.parity_error
                   ; tx_char_ready = char_tx.char_ready
                   ; tx_credit_init = of_int ~width:3 7 }
  in

  (* ── SPI slave ────────────────────────────────────────────────────── *)
  let spi_rdata_r = wire spi_data_bits -- "spi_rdata" in
  let spi = Spw_spi_slave.create
    Spw_spi_slave.I.{ clock = clk_sys; clear
                    ; sck = i.spi_sck; cs_n = i.spi_cs_n; mosi = i.spi_mosi
                    ; reg_rdata = spi_rdata_r }
  in

  (* ── Register file ────────────────────────────────────────────────── *)
  let link_up_pulse   = rising_edge (reg_spec ~clock:clk_sys ~clear ())
    (link_fsm.link_state ==: of_int ~width:Link_state.bits Link_state.run)
    -- "link_up_pulse"
  in
  let link_down_pulse = rising_edge (reg_spec ~clock:clk_sys ~clear ())
    (link_fsm.link_state <>: of_int ~width:Link_state.bits Link_state.run)
    -- "link_down_pulse"
  in

  let regfile = Spw_regfile.create
    Spw_regfile.I.{ clock = clk_sys; clear
                  ; spi_addr  = spi.reg_addr
                  ; spi_wdata = spi.reg_wdata
                  ; spi_wr_en = spi.wr_en
                  ; spi_rd_en = spi.rd_en
                  ; link_state     = link_fsm.link_state
                  ; disconnect_err = link_fsm.disconnect_err
                  ; parity_err     = link_fsm.parity_err_out
                  ; credit_err     = link_fsm.credit_err
                  ; link_up_pulse
                  ; link_down_pulse
                  ; rx_fifo_data   = rx_fifo.rd_data
                  ; rx_fifo_valid  = rx_fifo.rd_valid
                  ; tx_fifo_ready  = tx_fifo.wr_ready
                  ; rx_timecode    = gnd -- "rx_tc_stub" |> uresize 8
                  ; rx_timecode_valid = gnd }
  in
  spi_rdata_r        <== regfile.spi_rdata;
  link_en_r          <== regfile.link_en;
  tx_fifo_wr_data    <== regfile.tx_fifo_data;
  tx_fifo_wr_en      <== regfile.tx_fifo_wr_en;
  rx_fifo_rd_ready   <== regfile.rx_fifo_rd_en;

  (* ── Loopback mux (for self-test) ────────────────────────────────── *)
  let d_out = mux2 regfile.loopback enc.d_out enc.d_out -- "d_out" in
  let s_out = mux2 regfile.loopback enc.s_out enc.s_out -- "s_out" in

  { O.
    spw_d_p  = d_out
  ; spw_d_n  = ~: d_out
  ; spw_s_p  = s_out
  ; spw_s_n  = ~: s_out
  ; spi_miso = spi.miso
  ; irq_n    = ~: regfile.irq
  ; led_link = link_fsm.link_state ==: of_int ~width:Link_state.bits Link_state.run
  }

let circuit () =
  let module C = Circuit.With_interface (I) (O) in
  C.create_exn ~name:"spw_top" create
