(** Spw_regfile — Register file and control/status interconnect.

    This module sits between the SPI slave and the SpaceWire core.
    It implements every register in Spw_constants.Reg and produces the
    IRQ output line for the host MCU.

    All logic runs in [clk_sys] (the SPI / system clock domain).
    SpaceWire status signals that originate in the TX/RX bit-clock domains
    must already have been synchronised by the caller before connecting here.

    Register map: see [Spw_constants.Reg] for full documentation. *)

open Hardcaml
open Signal
open Hardcaml.Always
open Spw_constants

module I = struct
  type 'a t =
    { clock        : 'a [@bits 1]
    ; clear        : 'a [@bits 1]
    (* From SPI slave *)
    ; spi_addr     : 'a [@bits spi_addr_bits]
    ; spi_wdata    : 'a [@bits spi_data_bits]
    ; spi_wr_en    : 'a [@bits 1]
    ; spi_rd_en    : 'a [@bits 1]
    (* From SpW link FSM (synchronised) *)
    ; link_state   : 'a [@bits Link_state.bits]
    ; disconnect_err : 'a [@bits 1]
    ; parity_err   : 'a [@bits 1]
    ; credit_err   : 'a [@bits 1]
    ; link_up_pulse  : 'a [@bits 1]   (** single pulse when Run entered *)
    ; link_down_pulse: 'a [@bits 1]   (** single pulse when Run exited  *)
    (* From RX FIFO (sys-clock domain) *)
    ; rx_fifo_data  : 'a [@bits 9]    (** [8]=flag, [7:0]=payload *)
    ; rx_fifo_valid : 'a [@bits 1]    (** FIFO non-empty *)
    (* TX FIFO write interface *)
    ; tx_fifo_ready : 'a [@bits 1]    (** TX FIFO has space *)
    (* Timecode received (synchronised) *)
    ; rx_timecode   : 'a [@bits 8]
    ; rx_timecode_valid : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { spi_rdata     : 'a [@bits spi_data_bits]  (** to MISO shift register *)
    (* CTRL register outputs *)
    ; link_en       : 'a [@bits 1]
    ; loopback      : 'a [@bits 1]
    ; soft_reset    : 'a [@bits 1]
    (* TX FIFO write *)
    ; tx_fifo_data  : 'a [@bits 9]
    ; tx_fifo_wr_en : 'a [@bits 1]
    (* RX FIFO read *)
    ; rx_fifo_rd_en : 'a [@bits 1]
    (* Timecode transmit *)
    ; tx_timecode   : 'a [@bits 8]
    ; tx_timecode_en: 'a [@bits 1]
    (* IRQ line (active high, level) *)
    ; irq           : 'a [@bits 1]
    ; link_cfg      : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

let create (i : _ I.t) : _ O.t =
  let spec = reg_spec ~clock:i.clock ~clear:i.clear () in

  (* ── Register storage ─────────────────────────────────────────────── *)
  let reg_ctrl     = Variable.reg ~width:8 spec ~enable:vdd in
  let reg_irq_en   = Variable.reg ~width:8 spec ~enable:vdd in
  let reg_irq_stat = Variable.reg ~width:8 spec ~enable:vdd in
  let reg_timecode = Variable.reg ~width:8 spec ~enable:vdd in
  let reg_link_cfg = Variable.reg ~width:8 spec ~enable:vdd in
  let reg_rx_ctrl  = Variable.reg ~width:4 spec ~enable:vdd in

  (* ── Derived CTRL bits ─────────────────────────────────────────────── *)
  let link_en    = bit reg_ctrl.value 0 -- "link_en"    in
  let loopback   = bit reg_ctrl.value 1 -- "loopback"   in
  let soft_reset = Variable.wire ~default:gnd            in

  (* ── IRQ source assembly ──────────────────────────────────────────── *)
  (* Bit positions match Reg.irq_status documentation *)
  let irq_sources = concat_lsb
    [ i.rx_fifo_valid            (* 0: rx_avail     *)
    ; ~: i.tx_fifo_ready         (* 1: tx_empty     *)
    ; i.link_up_pulse            (* 2: link_up      *)
    ; i.link_down_pulse          (* 3: link_down    *)
    ; i.parity_err               (* 4: parity_err   *)
    ; i.disconnect_err           (* 5: disconnect   *)
    ; i.credit_err               (* 6: credit_err   *)
    ; gnd                        (* 7: reserved     *)
    ] -- "irq_sources"
  in

  (* ── Read-data mux ────────────────────────────────────────────────── *)
  let rd_mux_out = Variable.wire ~default:(zero spi_data_bits) in

  (* ── TX FIFO write wires ──────────────────────────────────────────── *)
  let tx_wr_en     = Variable.wire ~default:gnd in
  let tx_wr_data   = Variable.wire ~default:(zero 9) in
  let tx_timecode_en = Variable.wire ~default:gnd in
  let rx_rd_en     = Variable.wire ~default:gnd in

  compile [
    soft_reset     <-- gnd;
    tx_wr_en       <-- gnd;
    tx_wr_data     <-- zero 9;
    tx_timecode_en <-- gnd;
    rx_rd_en       <-- gnd;

    (* ── Latch IRQ status bits as they fire ── *)
    reg_irq_stat <-- reg_irq_stat.value |: irq_sources;

    (* ── Latch received timecode ── *)
    when_ i.rx_timecode_valid [
      reg_timecode <-- i.rx_timecode
    ];

    (* ── Latch RX character type alongside RX FIFO read ── *)
    when_ i.rx_fifo_valid [
      reg_rx_ctrl <-- sel_bottom i.rx_fifo_data 4
    ];

    (* ─────────── SPI Write path ─────────────────────────────────── *)
    when_ i.spi_wr_en [
      (* CTRL *)
      when_ (i.spi_addr ==: of_int ~width:spi_addr_bits Reg.ctrl) [
        reg_ctrl <-- i.spi_wdata;
        (* Bit 3 is soft_reset (W1C) — pulse once then clear *)
        when_ (bit i.spi_wdata 3) [
          soft_reset <-- vdd;
          reg_ctrl   <-- i.spi_wdata &: of_int ~width:8 0b11110111
        ]
      ];
      (* TX_DATA *)
      when_ (i.spi_addr ==: of_int ~width:spi_addr_bits Reg.tx_data) [
        when_ i.tx_fifo_ready [
          tx_wr_en   <-- vdd;
          tx_wr_data <-- concat_msb [gnd; i.spi_wdata]  (* flag=0 = data char *)
        ]
      ];
      (* TX_CTRL *)
      when_ (i.spi_addr ==: of_int ~width:spi_addr_bits Reg.tx_ctrl) [
        (* Bit 0: EOP, Bit 1: EEP, Bit 2: timecode *)
        when_ (bit i.spi_wdata 0) [
          tx_wr_en   <-- vdd;
          tx_wr_data <-- of_int ~width:9 0b100000010  (* flag=1, ctrl=EOP *)
        ];
        when_ (bit i.spi_wdata 1) [
          tx_wr_en   <-- vdd;
          tx_wr_data <-- of_int ~width:9 0b100000100  (* flag=1, ctrl=EEP *)
        ];
        when_ (bit i.spi_wdata 2) [
          tx_timecode_en <-- vdd
        ]
      ];
      (* TIMECODE *)
      when_ (i.spi_addr ==: of_int ~width:spi_addr_bits Reg.timecode) [
        reg_timecode <-- i.spi_wdata
      ];
      (* IRQ_EN *)
      when_ (i.spi_addr ==: of_int ~width:spi_addr_bits Reg.irq_en) [
        reg_irq_en <-- i.spi_wdata
      ];
      (* IRQ_STATUS: W1C — clear bits written as 1 *)
      when_ (i.spi_addr ==: of_int ~width:spi_addr_bits Reg.irq_status) [
        reg_irq_stat <-- reg_irq_stat.value &: (~: i.spi_wdata)
      ];
      (* LINK_CFG *)
      when_ (i.spi_addr ==: of_int ~width:spi_addr_bits Reg.link_cfg) [
        reg_link_cfg <-- i.spi_wdata
      ];
    ];

    (* ─────────── SPI Read path ───────────────────────────────────── *)
    when_ i.spi_rd_en [
      rd_mux_out <-- (
        mux i.spi_addr
          [ (* 0x00 *) reg_ctrl.value
          ; (* 0x01 *) concat_lsb
              [ uresize i.link_state 3
              ; i.rx_fifo_valid
              ; i.tx_fifo_ready
              ; i.disconnect_err
              ; i.parity_err
              ; i.credit_err
              ]
          ; (* 0x02 *) zero 8   (* TX_DATA is write-only *)
          ; (* 0x03 *) zero 8   (* TX_CTRL is write-only *)
          ; (* 0x04 *) ( rx_rd_en <-- vdd
                       ; uresize (sel_bottom i.rx_fifo_data 8) spi_data_bits )
          ; (* 0x05 *) uresize reg_rx_ctrl.value spi_data_bits
          ; (* 0x06 *) reg_timecode.value
          ; (* 0x07 *) reg_irq_en.value
          ; (* 0x08 *) reg_irq_stat.value
          ; (* 0x09 *) reg_link_cfg.value
          ]
      )
    ];
  ];

  { O.
    spi_rdata     = rd_mux_out.value
  ; link_en
  ; loopback
  ; soft_reset    = soft_reset.value
  ; tx_fifo_data  = tx_wr_data.value
  ; tx_fifo_wr_en = tx_wr_en.value
  ; rx_fifo_rd_en = rx_rd_en.value
  ; tx_timecode   = reg_timecode.value
  ; tx_timecode_en = tx_timecode_en.value
  ; irq = (reg_irq_stat.value &: reg_irq_en.value) <>: zero 8
  ; link_cfg = reg_link_cfg.value
  }

let circuit () =
  let module C = Circuit.With_interface (I) (O) in
  C.create_exn ~name:"spw_regfile" create
