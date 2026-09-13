(* generate.ml *)
open Hardcaml
open Project06_lib

module My_config = struct
  (* 1280x720 @ 60 Hz (CEA-861-D format 4). Pixel clock must match what
     lib/top.ml's rPLL (IDIV_SEL=3, FBDIV_SEL=54, ODIV_SEL=2) + CLKDIV
     (DIV_MODE=5) produce: 27 MHz * 55 / 4 / 5 = 74.25 MHz. *)
  let clk_fre = 74_250_000 (* Hz *)
  let h_total  = 1650
  let h_sync   = 40
  let h_bporch = 220
  let h_res    = 1280
  let v_total  = 750
  let v_sync   = 5
  let v_bporch = 20
  let v_res    = 720
  let hs_pol   = true
  let vs_pol   = true
  let is_simulation = false
  let pattern = [0;1;2;3;4;5;6;7]
end

let () =
  let module MyHdmi = Top.Make(My_config) in
  let module TopCircuit = Circuit.With_interface(MyHdmi.I)(MyHdmi.O) in
  let scope = Scope.create ~flatten_design:false () in
  (* let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () *)
  let circuit = TopCircuit.create_exn ~name:"top_level" (MyHdmi.create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the verilog code *)
  let output_dir = "verilog_out" in
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit
