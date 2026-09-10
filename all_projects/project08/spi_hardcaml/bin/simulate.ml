(* simulation.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project08_lib

module Common_config = struct
  let file_name = "image.hex"
  let startup_wait = 10
  let clk_div = 4 (* SPI clock = 27MHz / 4 *)
  let commands = [ 0xAE; 0x80; 0xAF ]
  let is_simulation = true
end

(* Nano 20K reset button is wired active-high (see tangnano20k.cst). *)
module Config_nano20k = struct
  include Common_config
  let normalize_reset x = x
end

(* Nano 4K reset button is wired active-low, pulled up (see tangnano4k.cst). *)
module Config_nano4k = struct
  include Common_config
  let normalize_reset x = Hardcaml.Signal.(~: x)
end

let testbench (module X : Config.S) ~idle_reset n =
  let module MyScreen = Top.Make (X) in
  let module Simulator = Cyclesim.With_interface (MyScreen.I) (MyScreen.O) in
  let scope =
    Scope.create
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let oc = open_out "screen.vcd" in
  let sim =
    Simulator.create
      ~config:Cyclesim.Config.trace_all (MyScreen.create scope) |> Vcd.wrap oc in
  let inputs : _ MyScreen.I.t = Cyclesim.inputs sim in
  let _outputs : _ MyScreen.O.t = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in

  (* Drive the raw (un-normalized) button pin at its idle level, which is
     board-dependent: the Nano 20K's i_reset idles low (active-high button),
     the Nano 4K's idles high (active-low button, pulled up). *)
  inputs.i_reset := idle_reset;

  for _i = 0 to n do
    Cyclesim.cycle sim
  done;
  close_out oc;
  waves

let () =
  let board = Option.value (Sys.getenv_opt "BOARD") ~default:"nano20k" in
  let config, idle_reset =
    match board with
    | "nano20k" -> (module Config_nano20k : Config.S), Bits.gnd
    | "nano4k" -> (module Config_nano4k : Config.S), Bits.vdd
    | other -> failwith (Printf.sprintf "Unknown BOARD '%s'; expected nano20k or nano4k" other)
  in
  let waves = testbench config ~idle_reset 1000 in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves
