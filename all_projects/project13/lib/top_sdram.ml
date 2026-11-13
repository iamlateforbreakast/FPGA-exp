open Hardcaml
open Signal
open Base

(** Tang Nano 20K top level: 27 MHz oscillator -> PLL -> SDRAM system, with
    the SDRAM DQ bus wrapped in IOBUFs.  The generated Verilog's
    [IO_sdram_dq] port is rewritten from output to inout by
    bin/generate.ml, exactly as project07 does for its I2C SDA pin. *)
module Make (X : Config_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; i_reset : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds : 'a [@bits X.led_width]
      ; io_sdram_dq : 'a [@bits 32] [@rtlname "IO_sdram_dq"]
      ; sdram_clk : 'a [@rtlname "O_sdram_clk"]
      ; sdram_cke : 'a [@rtlname "O_sdram_cke"]
      ; sdram_cs_n : 'a [@rtlname "O_sdram_cs_n"]
      ; sdram_ras_n : 'a [@rtlname "O_sdram_ras_n"]
      ; sdram_cas_n : 'a [@rtlname "O_sdram_cas_n"]
      ; sdram_wen_n : 'a [@rtlname "O_sdram_wen_n"]
      ; sdram_addr : 'a [@bits 11] [@rtlname "O_sdram_addr"]
      ; sdram_ba : 'a [@bits 2] [@rtlname "O_sdram_ba"]
      ; sdram_dqm : 'a [@bits 4] [@rtlname "O_sdram_dqm"]
      }
    [@@deriving hardcaml]
  end

  module Pll = Pll.Make (X)
  module Sys = Sdram_system.Make (X)

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
    let dq_in_w = Signal.wire 32 in
    let sys =
      Sys.hierarchical
        scope
        { Sys.I.clock = pll.clkout
        ; reset
        ; dq_in = dq_in_w
        }
    in
    let io_bits, in_bits =
      List.init 32 ~f:(fun k ->
        iobuf
          ~instance:(Printf.sprintf "sdram_dq%d" k)
          ~din:(select sys.dq_out k k)
          ~oe:sys.dq_oe)
      |> List.unzip
    in
    let io_sdram_dq = concat_lsb io_bits in
    let () = dq_in_w <== concat_lsb in_bits in
    { O.leds = sys.leds
    ; io_sdram_dq
    ; sdram_clk = sys.sdram_clk
    ; sdram_cke = sys.sdram_cke
    ; sdram_cs_n = sys.sdram_cs_n
    ; sdram_ras_n = sys.sdram_ras_n
    ; sdram_cas_n = sys.sdram_cas_n
    ; sdram_wen_n = sys.sdram_wen_n
    ; sdram_addr = sys.sdram_addr
    ; sdram_ba = sys.sdram_ba
    ; sdram_dqm = sys.sdram_dqm
    }
  ;;
end
