(* main.ml *)
open Hardcaml
open Project08_lib

module Common_config = struct
  let file_name = "image.hex"
  (* 270_000 cycles = 10ms at 27MHz, used for both the RES low pulse and the
     settle time after it. The SH1106 needs only microseconds; 10ms matches
     what the known-good reference driver uses and is generous. The previous
     10_000_000 (0.37s per phase, 0.74s before the first byte) bought nothing
     and made every hardware observation ambiguous, since a capture shorter
     than the delay is indistinguishable from a design that never starts. *)
  let startup_wait = 270_000
  let clk_div = 54
  (* Command list for an SH1106 driver (the common chip on 1.3-inch 128x64
     SPI OLEDs sold as SSD1306-compatible - it isn't quite: no horizontal
     addressing mode, and the charge pump is enabled via 0xAD/0x8B rather
     than SSD1306's 0x8D/0x14). Per-page RAM addressing (0xB0.., column
     commands) is sent separately by Top before each page of data. *)
  let commands = [
    0xAE; (* Display Off *)
    0xD5; 0x50; (* Clock Divide *)
    0xA8; 0x3F; (* Mux Ratio *)
    0xD3; 0x00; (* Offset *)
    0x40; (* Start Line *)
    0xAD; 0x8B; (* DC-DC (charge pump) ON *)
    0xA1; (* Segment Remap *)
    0xC8; (* COM Output Scan Direction *)
    0xDA; 0x12; (* COM Pins hardware config *)
    0xD9; 0x22; (* Pre-charge period *)
    0xDB; 0x35; (* VCOMH Deselect *)
    0x32; (* Set pump voltage: 8.0V *)
    0x81; 0xFF; (* Contrast *)
    0xA6; (* Normal Display *)
    0xA4; (* Resume RAM to Display. Swap to 0xA5 ("Entire Display ON") as a
             bring-up aid: it lights every pixel regardless of RAM contents,
             separating "panel accepts commands at all" from "RAM addressing
             is wrong". *)
    0xAF; (* Display On *)
  ]
  let col_offset = 2

  let is_simulation = false
end

(* Nano 20K reset button is wired active-high (see tangnano20k.cst). *)
module Config_nano20k = struct
  include Common_config
  let normalize_reset x = x
end

(* Nano 4K reset button (pin 15) is active-low - pulled up, pressing drives it
   to 0 - so it is inverted here into the active-high reset the design expects,
   the opposite of the 20K's wiring above.

   This pin previously had to be discarded because the constraint declared
   LVCMOS33 on it while pin 15 is Bank3, VCCIO 1.8V. Its pull-up could only
   reach 1.8V, under the ~2.0V VIH a 3.3V input buffer wants, so the released
   button read LOW or marginal - and because of the inversion below, a misread
   idle level became an *asserted* reset that parked every register and stopped
   the design dead, intermittently. The constraint is now LVCMOS18, where 1.8V
   is comfortably above the ~1.17V VIH, so the pin is trustworthy and the
   button is wired back in.

   Top ORs this with its power-on reset rather than relying on it: the POR is
   what guarantees a valid initial state after configuration, and the button
   only adds manual reset on top. *)
module Config_nano4k = struct
  include Common_config
  let normalize_reset pin = Hardcaml.Signal.(~: pin)
end

let generate (module X : Config.S) ~output_dir =
  let module MyScreen = Top.Make(X) in
  let module TopCircuit = Circuit.With_interface(MyScreen.I)(MyScreen.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"top_level" (MyScreen.create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the circuit *)
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit

let () =
  let board = Option.value (Sys.getenv_opt "BOARD") ~default:"nano20k" in
  (* Output is namespaced per board: the generated RTL differs (see
     normalize_reset), so a shared verilog_out/ would go stale silently
     when switching boards without touching any source file. *)
  let output_dir = "verilog_out/" ^ board in
  match board with
  | "nano20k" -> generate (module Config_nano20k : Config.S) ~output_dir
  | "nano4k" -> generate (module Config_nano4k : Config.S) ~output_dir
  | other -> failwith (Printf.sprintf "Unknown BOARD '%s'; expected nano20k or nano4k" other)
