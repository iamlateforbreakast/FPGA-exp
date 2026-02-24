(* simulate.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project07_lib

module My_config = struct
  let clk_fre = 27_000_000
  let uart_fre = 27_000_000
  let baud_rate = 115_200
  let i2c_address = 0x76
  let pattern = [0x21; 0x12; 0x0C; 0x00; 0x12; 0x21]
  let message = "Temp: XXXX Press: XXXX"
  let is_simulation = true
end

module MyBmp280 = I2c_master.Make(My_config)

module Simulator = Cyclesim.With_interface (MyBmp280.I)(MyBmp280.O)

let testbench n =
  let scope = Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let oc = open_out "bmp280.vcd" in
  let sim = Simulator.create
      ~config:Cyclesim.Config.trace_all (MyBmp280.create scope) |> Vcd.wrap oc in
  let inputs : _ MyBmp280.I.t = Cyclesim.inputs sim in
  let _outputs : _ MyBmp280.O.t = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in

  let cycle n =
    for _ = 1 to n do
      Cyclesim.cycle sim;
    done
  in
  
  (* Initialise inputs for write *)
  inputs.dev_addr := Bits.of_int ~width:7 0x76;
  inputs.reg_addr := Bits.of_int ~width:8 0xF4; (* BMP280 temperaturate register *)
  inputs.mosi := Bits.of_int ~width:8 0XAA;
  inputs.rw := Bits.gnd; (* Write = 0, Read = 1 *)
  inputs.start := Bits.gnd;
  inputs.sda_in := Bits.gnd;

  inputs.reset := Bits.gnd;
  cycle 1;
  inputs.reset := Bits.vdd;
  cycle 2;
  inputs.reset := Bits.gnd;
  cycle 2;
  inputs.start := Bits.vdd;
  cycle (375);
 
  inputs.dev_addr := Bits.of_int ~width:7 0x76;
  inputs.reg_addr := Bits.of_int ~width:8 0xF4; (* BMP280 temperaturate register *)
  inputs.mosi := Bits.of_int ~width:8 0X00; (* Unused *)
  inputs.rw := Bits.vdd; (* Write = 0, Read = 1 *)
  inputs.start := Bits.vdd;
  inputs.sda_in := Bits.gnd;

  cycle (n-375);

  close_out oc;
  waves

let () =
  let waves = testbench 2000 in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves
