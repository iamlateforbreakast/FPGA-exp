(* generate.ml *)
open Hardcaml
open Project06_lib

module Common_config = struct
  (* 640x480 @ 60 Hz (VESA VGA / CEA-861 format 1). Chosen so the SAME
     timing spec, and very nearly the same pixel clock, works on both
     boards: the Nano 4K's GW1NS-4 PLLVR only has a Sipeed-verified
     parameter set for this resolution (126 MHz serial clock / 5 = 25.2 MHz
     pixel clock - see Config_nano4k), and the Nano 20K's rPLL below is
     tuned to hit that same 126 MHz from its 27 MHz oscillator via the
     identical idiv/fbdiv ratio, just with its own odiv/device (see
     Config_nano20k). Canonical VESA pixel clock is 25.175 MHz; 25.2 MHz is
     the closest both PLLs can produce with integer dividers, matching what
     Sipeed's own reference design settles for on this exact chip. *)
  let clk_fre = 25_200_000 (* Hz *)
  let h_total  = 800
  let h_sync   = 96
  let h_bporch = 48
  let h_res    = 640
  let v_total  = 525
  let v_sync   = 2
  let v_bporch = 33
  let v_res    = 480
  let hs_pol   = false
  let vs_pol   = false
  let is_simulation = false
  let pattern = [0;1;2;3;4;5;6;7]
end

(* Nano 20K: I_rst is already active-high (pulled down when idle, see
   tangnano20k.cst), matching what lib/top.ml assumes - no correction
   needed. The 6 onboard LEDs are active-low (pulled up when idle, see
   tangnano20k.cst), hence the inversion.

   rPLL (GW2A-18C): idiv_sel=2, fbdiv_sel=13 -> CLKOUT = 27*14/3 = 126MHz, matching the Nano
   4K's PLLVR output exactly (same idiv/fbdiv ratio - see Config_nano4k).
   odiv_sel=4 -> VCO = CLKOUT*odiv_sel = 504MHz, comfortably inside this
   family's legal PLL range (the design's previous 720p config ran a
   742.5MHz VCO on the same primitive/device without issue). Verified by
   placing-and-routing with nextpnr-himbaechel in the fpga-exp-dev
   container: timing closes with no PLL-parameter errors. *)
module Config_nano20k = struct
  include Common_config
  let normalize_reset x = x
  let normalize_led x = Signal.(~:x)
  let led_width = 6
  (* The Nano 20K's HDMI pins are wired for the true-LVDS driver. *)
  let lvds_primitive = `TLVDS
  let pll_primitive = `RPLL
  let pll_idiv_sel = 2
  let pll_fbdiv_sel = 13
  let pll_odiv_sel = 4
  let gowin_device = "GW2A-18C"
end

(* Nano 4K: the onboard buttons are active-low (pulled up when idle, KEY1 on
   pin 15 - see tangnano4k.cst), the opposite of the 20K's wiring, hence the
   inversion here (mirrors project08/spi_hardcaml's Config_nano4k). The
   single onboard LED (pin 10) is driven directly with no inversion,
   matching project08's own validated wiring for this same pin on this same
   board - project08's heartbeat LED there is driven with no polarity flip
   and works.

   PLLVR (GW1NSR-4C): idiv_sel=2, fbdiv_sel=13, odiv_sel=8, device=
   "GW1NSR-4C" are exactly Sipeed's own verified values for this chip
   (github.com/sipeed/TangNano-4K-example, hdmi_720p/src/gowin_pllvr/
   gowin_pllvr.v) - CLKOUT = 27*14/3 = 126MHz, then /5 via CLKDIV = 25.2MHz
   pixel clock, matching Common_config's 640x480@60 timings. *)
module Config_nano4k = struct
  include Common_config
  let normalize_reset x = Signal.(~:x)
  let normalize_led x = x
  let led_width = 1
  (* Emulated LVDS - what Sipeed's working reference for this board uses on
     these same pins. See config_intf.ml's [lvds_primitive]. *)
  let lvds_primitive = `ELVDS
  let pll_primitive = `PLLVR
  let pll_idiv_sel = 2
  let pll_fbdiv_sel = 13
  let pll_odiv_sel = 8
  let gowin_device = "GW1NSR-4C"
end

let generate (module X : Config.S) ~output_dir =
  let module MyHdmi = Top.Make (X) in
  let module TopCircuit = Circuit.With_interface (MyHdmi.I) (MyHdmi.O) in
  let scope = Scope.create ~flatten_design:false () in
  (* let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () *)
  let circuit = TopCircuit.create_exn ~name:"top_level" (MyHdmi.create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the verilog code *)
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output ~database Verilog ~output_mode:(In_directory output_dir) circuit

let () =
  let board = Option.value (Sys.getenv_opt "BOARD") ~default:"nano20k" in
  (* Output is namespaced per board, matching project08/spi_hardcaml: the
     generated RTL differs per board (different PLL primitive, LED width),
     so a shared verilog_out/ would go stale silently when switching boards
     without touching any source file. *)
  let output_dir = "verilog_out/" ^ board in
  match board with
  | "nano20k" -> generate (module Config_nano20k : Config.S) ~output_dir
  | "nano4k" -> generate (module Config_nano4k : Config.S) ~output_dir
  | other -> failwith (Printf.sprintf "Unknown BOARD '%s'; expected nano20k or nano4k" other)
