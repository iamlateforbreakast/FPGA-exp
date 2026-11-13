open Hardcaml
open Project13_lib

(* 81 MHz fabric clock => 20.25 MHz SDRAM/HyperRAM CK.  The controllers use
   four fabric phases per memory clock.  This is the same memory clock as the
   reference HyperRAM controller's 81 MHz logic clock, with a comfortable
   timing margin on both the GW2AR-18 and the smaller GW1NS-4. *)
module Common = struct
  let clock_freq = 27_000_000
  let pll_freq = 81_000_000
  let is_simulation = false
  let pll_idiv_sel = 0 (* /1 *)
  let pll_fbdiv_sel = 2 (* x3 => 81 MHz *)
  let pll_odiv_sel = 8
end

(* Tang Nano 20K: active-high reset button, active-low LEDs, rPLL. *)
module Config_nano20k = struct
  include Common
  let normalize_reset x = x
  let normalize_led x = Hardcaml.Signal.(~: x)
  let led_width = 6
  let pll_primitive = `RPLL
  let gowin_device = "GW2A-18C"
end

(* Tang Nano 4K: active-low reset button (pin 15), active-high LED, PLLVR. *)
module Config_nano4k = struct
  include Common
  let normalize_reset x = Hardcaml.Signal.(~: x)
  let normalize_led x = x
  let led_width = 1
  let pll_primitive = `PLLVR
  let gowin_device = "GW1NSR-4C"
end

let make_inout path patterns =
  let args =
    List.concat (List.map (fun (width, name) ->
      [ "-e"
      ; Filename.quote
          (Printf.sprintf
             "s/output \\[%d:0\\] %s/inout [%d:0] %s/"
             width
             name
             width
             name)
      ]) patterns)
  in
  let cmd =
    String.concat " " ([ "sed"; "-i" ] @ args @ [ Filename.quote path ])
  in
  let rc = Sys.command cmd in
  if rc <> 0 then failwith (Printf.sprintf "sed failed (%d): %s" rc cmd)
;;

let generate (module X : Config_intf.S) ~output_dir ~inout_ports =
  let module Top = Top_sdram.Make (X) in
  let module TopCircuit = Circuit.With_interface (Top.I) (Top.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"top_level" (Top.create scope) in
  let database = Scope.circuit_database scope in
  let _ = Sys.command ("mkdir -p " ^ Filename.quote output_dir) in
  Rtl.output
    ~database
    Verilog
    ~output_mode:(In_directory output_dir)
    circuit;
  make_inout (output_dir ^ "/top_level.v") inout_ports
;;

let generate_hyperram (module X : Config_intf.S) ~output_dir ~inout_ports =
  let module Top = Top_hyperram.Make (X) in
  let module TopCircuit = Circuit.With_interface (Top.I) (Top.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"top_level" (Top.create scope) in
  let database = Scope.circuit_database scope in
  let _ = Sys.command ("mkdir -p " ^ Filename.quote output_dir) in
  Rtl.output
    ~database
    Verilog
    ~output_mode:(In_directory output_dir)
    circuit;
  make_inout (output_dir ^ "/top_level.v") inout_ports
;;

let () =
  let board = Option.value (Sys.getenv_opt "BOARD") ~default:"nano20k" in
  let output_dir = "verilog_out/" ^ board in
  match board with
  | "nano20k" ->
    generate
      (module Config_nano20k : Config_intf.S)
      ~output_dir
      ~inout_ports:[ 31, "IO_sdram_dq" ]
  | "nano4k" ->
    generate_hyperram
      (module Config_nano4k : Config_intf.S)
      ~output_dir
      ~inout_ports:[ 7, "IO_hpram_dq"; 1, "IO_hpram_rwds" ]
  | other ->
    failwith
      (Printf.sprintf "Unknown BOARD '%s'; expected nano20k or nano4k" other)
;;
