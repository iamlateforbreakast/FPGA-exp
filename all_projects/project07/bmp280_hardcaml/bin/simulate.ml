(* simulate.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project07_lib

module My_config = struct
  let clk_fre = 27
  let i2c_address = 0x76
  let is_simulation = true
end

module MyBmp280 = Top.Make(My_config)

module Simulator = Cyclesim.With_interface (MyBmp280.I)(MyBmp280.O)

let testbench n =
  let scope = 
    Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let oc = open_out "bmp280.vcd" in
  let sim = 
    Simulator.create
      ~config:Cyclesim.Config.trace_all (MyBmp280.create scope) |> Vcd.wrap oc in
  let inputs : _ MyBmp280.I.t = Cyclesim.inputs sim in
  let _outputs : _ MyBmp280.O.t = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in

  inputs.reset := Bits.gnd;

  for _i = 0 to n do
    Cyclesim.cycle sim
  done;
  close_out oc;
  waves

let () =
  let waves = testbench 1000 in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves
