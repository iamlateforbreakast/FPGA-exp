(* test_vesa.ml *)

open Hardcaml
open Project06_lib

module My_config = struct
  let svo_mode = "640x480"
end

module MyVesa = Vesa.Make(My_config)

module Simulator = Cyclesim.With_interface(MyVesa.I)(MyVesa.O)


let testbench () =
  let scope = Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let sim = Simulator.create (MyVesa.create scope) in
  let inputs : _ MyVesa.I.t = Cyclesim.inputs sim in
  let outputs : _ MyVesa.O.t = Cyclesim.outputs sim in
  let _step () =
    inputs.i_resetn := Bits.vdd;
    Printf.printf "o_column=%i o_row=%i o_vsync=%i o_hsync=%i o_data_en=%i\n"
      (Bits.to_int !(outputs.o_column))
      (Bits.to_int !(outputs.o_row))
      (Bits.to_int !(outputs.o_vsync))
      (Bits.to_int !(outputs.o_hsync))
      (Bits.to_int !(outputs.o_data_en));
  in
  for cycle = 0 to 1000 do
    Cyclesim.cycle sim;
  done;

  let%expect_test "vesa" =
  testbench ();
  [%expect {|
    o_column=0 o_row=0 o_vsync=0 o_hsync=0 o_data_en=0
    o_column=0 o_row=0 o_vsync=0 o_hsync=0 o_data_en=0
    o_column=0 o_row=0 o_vsync=0 o_hsync=0 o_data_en=0
    o_column=0 o_row=0 o_vsync=0 o_hsync=0 o_data_en=0
    o_column=0 o_row=0 o_vsync=0 o_hsync=0 o_data_en=0
    o_column=0 o_row=0 o_vsync=0 o_hsync=0 o_data_en=0
  |}]