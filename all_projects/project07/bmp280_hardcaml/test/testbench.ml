open Hardcaml
open Hardcaml.Bits

module Sim = Cyclesim.With_interface(I2c_master.I)(I2c_master.O)

let run_test () =
  (* Create the simulator *)
  let sim = Sim.create (I2c_master.create (Scope.create ())) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  (* Helper to pulse clock and print status *)
  let cycle n =
    for _ = 1 to n do
      Cyclesim.cycle sim;
    done
  in

  (* 1. Reset the system *)
  inputs.reset := vdd;
  cycle 2;
  inputs.reset := gnd;

  (* 2. Configure a Read transaction *)
  inputs.addr     := of_int ~w:7 0x50; (* Slave Address *)
  inputs.reg_addr := of_int ~w:8 0x12; (* Sub-address to read *)
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

  Printf.printf "Final Data Received: 0x%x\n" (to_int !(outputs.dout))

