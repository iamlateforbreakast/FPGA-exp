(* testbench.ml *)
open Hardcaml
open Project8_lib

module My_config = struct
  let file_name = "image.hex"
  let startup_wait = 10
  let clk_div = 4 
  let commands = [ 0xAE; 0x80; 0xAF ]
end

module MyScreen = Screen.Make(My_config)
module Simulator = Cyclesim.With_interface(MyScreen.I)(MyScreen.O)

let testbench () =
  let scope = Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let sim = Simulator.create (MyScreen.create scope) in
  let _inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  (* Define what happens every cycle *)
  let print_outputs () =
    Printf.printf "io_sclk=%i io_sdin=%i io_cs=%i io_dc=%i io_reset=%i\n"
      (Bits.to_int !(outputs.io_sclk))
      (Bits.to_int !(outputs.io_sdin))
      (Bits.to_int !(outputs.io_cs))
      (Bits.to_int !(outputs.io_dc))
      (Bits.to_int !(outputs.io_reset))
  in
  
  (* Run the simulation loop *)
  for _cycle = 0 to My_config.startup_wait * 3 do
    Cyclesim.cycle sim;
    print_outputs (); (* Call the print function each cycle *)
  done

let%expect_test "screen" =
  testbench ();
  [%expect {|
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
    io_sclk=0 io_sdin=0 io_cs=0 io_dc=0 io_reset=0
  |}]
