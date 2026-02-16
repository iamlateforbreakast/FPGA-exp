(* testbench.ml *)
open Hardcaml
open Project07_lib
open Hardcaml.Bits

module My_config = struct
  let clk_fre = 27_000_000
  let uart_fre = 27_000_000
  let baud_rate = 115_200
  let i2c_address = 0x76
  let pattern = [0;1;2;3;4;5;6;7]
  let is_simulation = false
end

module MyI2c_master = I2c_master.Make(My_config)
module Sim = Cyclesim.With_interface(MyI2c_master.I)(MyI2c_master.O)

let testbench () =
  let scope = Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let sim = Sim.create (MyI2c_master.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  (* Helper to pulse clock and print status *)
  let cycle n =
    for _ = 1 to n do
      Cyclesim.cycle sim;
    done
  in

  Printf.printf "%s\n" "Hello";

  (* 1. Reset the system *)
  inputs.reset := vdd;
  cycle 2;
  inputs.reset := gnd;

  (* 2. Configure a Read transaction *)
  inputs.dev_addr     := of_int ~width:7 0x50; (* Slave Address *)
  inputs.reg_addr := of_int ~width:8 0x12; (* Sub-address to read *)
  inputs.mosi := Bits.of_int ~width:8 0;
  inputs.rw       := vdd;              (* Read = 1 *)
  inputs.start    := vdd;
  cycle 1;
  inputs.start    := gnd;

  (* 3. Run until the Slave needs to ACK the address *)
  (* You must wait long enough for the 8 bits + timing to pass *)
  cycle 2000; 

  (* 4. Mock a Slave ACK: pull SDA low *)
  inputs.sda_in := gnd; 
  cycle 500; (* Hold for one SCL pulse *)
  inputs.sda_in := vdd;

  (* 5. Wait for transaction to complete and check result *)
  while not (to_bool !(outputs.ready)) do
    cycle 10;
  done;

  Printf.printf "Final Data Received: 0x%x\n" (to_int !(outputs.miso))

let%expect_test "bmp280" =
  testbench ();
  [%expect {|
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
  |}]
