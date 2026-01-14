(* testbench.ml *)

open Hardcaml
open Project04_lib

module My_config = struct
  let clk_fre = 27
  let cycle_delay = 27_000_000
  let ws2812_num = 0
  let ws2812_width = 6
  let colors = [ 0xFF0000; 0x00FF00; 0x0000FF ]  (* Red, Green, Blue *)
end

module MyWs2812 = Ws2812.Make(My_config)

module Simulator = Cyclesim.With_interface(MyWs2812.I)(MyWs2812.O)


let testbench () =
  let scope = Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let sim = Simulator.create (MyWs2812.create scope) in
  let _inputs : _ MyWs2812.I.t = Cyclesim.inputs sim in
  let outputs : _ MyWs2812.O.t = Cyclesim.outputs sim in
  let step () =
    let _ = Printf.printf "data=%i\n"
      (Bits.to_int !(outputs.data));

  let check_init_state _cycle =
    assert (Bits.to_int !(outputs.data) = 1);
    () in

  for cycle = 0 to 27 do
    Cyclesim.cycle sim;
    check_init_state cycle;
  done;

let%expect_test "ws2812" =
  testbench ();
  [%expect {|
    data=1
    data=1
    data=1
    data=1
    data=1
    data=1
  |}]
