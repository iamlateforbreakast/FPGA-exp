open Hardcaml
open Signal
open Base

(** Tang Nano 4K top level: 27 MHz oscillator -> PLLVR -> HyperRAM system,
    with DQ/RWDS wrapped in IOBUFs.  The generated Verilog's
    [IO_hpram_dq] and [IO_hpram_rwds] ports are rewritten from output to
    inout by bin/generate.ml.

    apicula/nextpnr model the GW1NS-4 HyperRAM pseudo-pins as two-entry
    vectors ([O_hpram_ck[1:0]], ...).  The board only has one x8 device, so
    both entries carry the same CK/CS/reset value and only RWDS[0] is
    connected.  RWDS[1] is left high-Z with a TBUF. *)
module Make (X : Config_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; i_reset : 'a
      ; i_sclk : 'a
      ; i_mosi : 'a
      ; i_cs_n : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_led : 'a [@bits X.led_width] [@rtlname "O_led"]
      ; o_miso : 'a
      ; io_hpram_dq : 'a [@bits 8] [@rtlname "IO_hpram_dq"]
      ; io_hpram_rwds : 'a [@bits 2] [@rtlname "IO_hpram_rwds"]
      ; o_hpram_ck : 'a [@bits 2] [@rtlname "O_hpram_ck"]
      ; o_hpram_ck_n : 'a [@bits 2] [@rtlname "O_hpram_ck_n"]
      ; o_hpram_cs_n : 'a [@bits 2] [@rtlname "O_hpram_cs_n"]
      ; o_hpram_reset_n : 'a [@bits 2] [@rtlname "O_hpram_reset_n"]
      }
    [@@deriving hardcaml]
  end

  module Pll = Pll.Make (X)
  module Sys = Hyperram_system.Make (X)

  module Debug = Debug_spi.Make (struct
      let trace_depth = 512
    end)

  let iobuf ~instance ~din ~oe =
    let m =
      Instantiation.create
        ~name:"IOBUF"
        ~instance
        ~inputs:[ "I", din; "OEN", ~: oe ]
        ~outputs:[ "IO", 1; "O", 1 ]
        ()
    in
    Map.find_exn m "IO", Map.find_exn m "O"
  ;;

  let tbuf ~instance ~din ~oe =
    let m =
      Instantiation.create
        ~name:"TBUF"
        ~instance
        ~inputs:[ "I", din; "OEN", oe ]
        ~outputs:[ "O", 1 ]
        ()
    in
    Map.find_exn m "O"
  ;;

  let create scope (i : Signal.t I.t) : Signal.t O.t =
    let pll = Pll.hierarchical scope { Pll.I.clkin = i.clock } in
    let base_rst = X.normalize_reset i.i_reset |: ~:(pll.lock) in
    let por_spec = Reg_spec.create ~clock:pll.clkout ~reset:base_rst () in
    let por_cnt =
      reg_fb
        por_spec
        ~enable:vdd
        ~width:16
        ~f:(fun c -> mux2 (msb c) c (c +:. 1))
    in
    let reset = base_rst |: ~: (msb por_cnt) in
    let debug_done_w = Signal.wire 1 in
    let dq_in_w = Signal.wire 8 in
    let rwds_in_w = Signal.wire 1 in
    let sys =
      Sys.hierarchical
        scope
        { Sys.I.clock = pll.clkout
        ; reset
        ; dq_in = dq_in_w
        ; rwds_in = rwds_in_w
        }
    in
    let dq_io_bits, dq_in_bits =
      List.init 8 ~f:(fun k ->
        iobuf
          ~instance:(Printf.sprintf "hpram_dq%d" k)
          ~din:(select sys.dq_out k k)
          ~oe:sys.dq_oe)
      |> List.unzip
    in
    let rwds_io_0, rwds_in_0 =
      iobuf ~instance:"hpram_rwds0" ~din:sys.rwds_out ~oe:sys.rwds_oe
    in
    let rwds_io_1 = tbuf ~instance:"hpram_rwds1" ~din:gnd ~oe:vdd in
    let io_hpram_dq = concat_lsb dq_io_bits in
    let io_hpram_rwds = concat_lsb [ rwds_io_0; rwds_io_1 ] in
    let () =
      dq_in_w <== concat_lsb dq_in_bits;
      rwds_in_w <== rwds_in_0
    in
    (* Logic-analyser trace of the first read transaction (from its CS#
       falling edge), read out by the Pi over SPI.  See Debug_spi and
       raspberry_pi/read_trace.py. *)
    let dspec = Reg_spec.create ~clock:pll.clkout ~reset () in
    let cs_n_d = reg dspec sys.hpram_cs_n in
    let in_read = (sys.test_state ==:. 5) |: (sys.test_state ==:. 6) in
    let debug =
      Debug.hierarchical
        scope
        { Debug.I.clock = pll.clkout
        ; reset
        ; sclk = i.i_sclk
        ; cs_n = i.i_cs_n
        ; trigger = cs_n_d &: ~:(sys.hpram_cs_n) &: in_read
        ; probe =
            concat_msb
              [ sys.hpram_ck
              ; sys.hpram_cs_n
              ; rwds_in_0
              ; sys.dq_oe
              ; sys.rwds_oe
              ; sys.rwds_out
              ; Signal.zero 2
              ; concat_lsb dq_in_bits
              ]
        ; header =
            concat_msb
              [ of_int ~width:8 0xA5
              ; uresize (concat_msb [ debug_done_w; sys.fail; sys.pass ]) 8
              ; uresize sys.test_state 8
              ; uresize sys.test_addr 24
              ; sys.dout
              ; Signal.zero (8 * 8)
              ]
        }
    in
    debug_done_w <== debug.trace_done;
    let ck = sys.hpram_ck in
    let cs_n = sys.hpram_cs_n in
    let rst_n = sys.hpram_reset_n in
    { O.o_led = sys.led
    ; o_miso = debug.miso
    ; io_hpram_dq
    ; io_hpram_rwds
    ; o_hpram_ck = concat_lsb [ ck; ck ]
    ; o_hpram_ck_n = concat_lsb [ ~: ck; ~: ck ]
    ; o_hpram_cs_n = concat_lsb [ cs_n; cs_n ]
    ; o_hpram_reset_n = concat_lsb [ rst_n; rst_n ]
    }
  ;;
end
