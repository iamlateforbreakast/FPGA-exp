(* testbench.ml *)
open Hardcaml
open Project07_lib
open Bmp280_model
open Hardcaml.Bits

module My_config = struct
  let clk_fre = 27_000_000
  let uart_fre = 27_000_000
  let baud_rate = 115_200
  let i2c_address = 0x76
  let pattern = [0;1;2;3;4;5;6;7]
  let message = "Temp: XXXX Press: XXXX"
  let is_simulation = true
end

module MyI2c_master = I2c_master.Make(My_config)
module Sim = Cyclesim.With_interface(MyI2c_master.I)(MyI2c_master.O)

let testbench () =
  Printf.printf "ENTERING TESTBENCH\n%!"; (* %! forces flush *)
  let scope = Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let base_sim = Sim.create ~config:Cyclesim.Config.trace_all (MyI2c_master.create scope) in
  let waves, sim = Hardcaml_waveterm.Waveform.create base_sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let model = Bmp280_Model.create ~addr:0x76 in

  (* Helper to pulse clock and print status *)
  let cycle n =
    for _ = 1 to n do
      let scl_bit = to_int !(outputs.scl) in
      let sda_out_bit = to_int !(outputs.sda_out) in
      let sda_oe_bit = to_int !(outputs.sda_oe) in
      let ack_bit = to_int !(outputs.ack_error) in
      let sda_in_bit = to_int !(inputs.sda_in) in
      
      let model_contribution = Bmp280_Model.step model ~scl:scl_bit ~sda_in:sda_in_bit in
      (* Combine for Open-Drain logic: Bus is low if either pulls down *)
      (*let bus_sda = if (sda_oe_bit = 1) then sda_out_bit else model_contribution in*)
      let bus_sda = if (sda_oe_bit = 1) then sda_out_bit else model_contribution in
      inputs.sda_in := (of_int ~width:1 bus_sda);

      Printf.printf "Cycle %d: SCL=%d SDA_out=%d SDA_OE=%d ACK=%d SDA_in=%d Model_contribution=%d\n" 
        model.cycle scl_bit sda_out_bit sda_oe_bit ack_bit sda_in_bit model_contribution;
      Cyclesim.cycle sim;
    done
  in

  Printf.printf "%s\n" "Hello";

  (* 1. Reset the system *)
  inputs.reset := vdd;
  cycle 2;
  inputs.reset := gnd;

  (* 2. Configure a Write transaction *)
  inputs.dev_addr := of_int ~width:7 My_config.i2c_address; (* Slave Address *)
  inputs.reg_addr := of_int ~width:8 0x27; (* Sub-address to read *)
  inputs.mosi     := Bits.of_int ~width:8 66;
  inputs.rw       := gnd;                  (* Write = 0 *)
  inputs.start    := vdd;
  cycle 1;

  (* 3. Run until the Slave needs to ACK the address *)
  (* You must wait long enough for the 8 bits + timing to pass *)
  cycle 50;

  (* 4. Wait for transaction to complete and check result *)
  while not (to_bool !(outputs.ready)) do
    cycle 5;
  done;

  (* 5. Configure a Read transaction *)
  inputs.dev_addr := of_int ~width:7 My_config.i2c_address; (* Slave Address *)
  inputs.reg_addr := of_int ~width:8 0x27; (* Sub-address to read *)
  inputs.mosi     := Bits.of_int ~width:8 0;
  inputs.rw       := vdd;                  (* Read = 1 *)
  inputs.start    := vdd;
  cycle 1;

  (* 6. Run until the Slave sends data back *)
  cycle 50;

  (* 7. Wait for transaction to complete and check result *)
  while not (to_bool !(outputs.ready)) do
    cycle 5;
  done;

  waves

let () = 
  let _waves = testbench () in ()
  (* Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves*)

(* let%expect_test "bmp280" =
  testbench ();
  [%expect {|
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
  |}]*)
       
