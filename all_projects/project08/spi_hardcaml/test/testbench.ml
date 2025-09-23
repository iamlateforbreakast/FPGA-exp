module Simulator = Cyclesim.With_interface(I)(O)

let testbench () =
  let sim = Simulator.create create in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let step ~clear ~incr =
    inputs.clear := if clear=1 then Bits.vdd else Bits.gnd;
    inputs.incr := if incr=1 then Bits.vdd else Bits.gnd;
    Printf.printf "clear=%i incr=%i dout=%i\n"
      clear incr (Bits.to_int !(outputs.dout));
    Cyclesim.cycle sim
  in
  step ~clear:0 ~incr:0;
  step ~clear:0 ~incr:1;
  step ~clear:0 ~incr:1;
  step ~clear:1 ~incr:0;
  step ~clear:0 ~incr:0;
  step ~clear:0 ~incr:0
;;
let%expect_test "counter" =
  testbench ();
  [%expect {|
    clear=0 incr=0 dout=0
    clear=0 incr=1 dout=0
    clear=0 incr=1 dout=1
    clear=1 incr=0 dout=2
    clear=0 incr=0 dout=0
    clear=0 incr=0 dout=0
  |}]
;;
