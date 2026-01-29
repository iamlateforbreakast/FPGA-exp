(* simulate.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project05_lib

module My_config = struct
  let clk_fre = 27_000_000 (* Hz *)
  let uart_fre = 50_000_000 (* Hz *)
  let baud_rate = 10_000_000 (* 5 *)
  let repeat_fre = 1 (* Hz *)
  let message = "Hello world!\n"
  let pattern = [ 0x20; 0x10; 0x08; 0x04; 0x02; 0x01; 0x02; 0x04; 0x08; 0x10 ]
  let is_simulation = true
end

module MyUart = Top.Make(My_config)

module Simulator = Cyclesim.With_interface (MyUart.I)(MyUart.O)

let testbench n =
  let scope = 
    Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let oc = open_out "uart.vcd" in
  let sim = 
    Simulator.create
      ~config:Cyclesim.Config.trace_all (MyUart.create scope) |> Vcd.wrap oc in
  let inputs : _ MyUart.I.t = Cyclesim.inputs sim in
  let _outputs : _ MyUart.O.t = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in

  inputs.reset := Bits.gnd;

  for _i = 0 to n do
    Cyclesim.cycle sim
  done;
  close_out oc;
  waves

let () =
  let waves = testbench 2000 in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves
