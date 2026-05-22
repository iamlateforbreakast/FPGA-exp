open Hardcaml_sim

open Base
open Hardcaml
open Signal

module Spw_interface = struct
  module I = struct
    type 'a t = {
      clk      : 'a;
      rst      : 'a;
      link_en  : 'a;
      link_dis : 'a;
      tx_write : 'a;
      tx_data  : 'a; [@bits 9]
      rx_read  : 'a;
      rx_d     : 'a;
      rx_s     : 'a;
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      link_active : 'a;
      link_error  : 'a;
      tx_ready    : 'a;
      rx_data     : 'a; [@bits 9]
      rx_valid    : 'a;
      tx_d        : 'a;
      tx_s        : 'a;
    } [@@deriving sexp_of, hardcaml]
  end
end
let create (i : Signal.t Spw_interface.I.t) =
  (* In a real app, you'd call your actual CODEC implementation here:
     let outputs = Spw_codec_implementation.create i in 
  *)
  
  (* Example of the loopback logic from your Verilog: 
     assign din = dout; assign sin = sout; 
     This is handled by passing the outputs back to the inputs in the simulator. *)
  
  (* Logic placeholder for the CODEC *)
  Spw_interface.O.{
    link_active = i.link_en; (* Dummy logic *)
    link_error  = i.link_dis;
    tx_ready    = i.clk; 
    rx_data     = i.tx_data;
    rx_valid    = i.tx_write;
    tx_d        = i.rx_d;
    tx_s        = i.rx_s;
  }

let testbench () =
  let module Sim = Cyclesim.With_interface (Spw_interface.I) (Spw_interface.O) in
  let sim = Sim.create create in
  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  (* Helper to cycle the clock *)
  let cycle () =
    Sim.cycle sim
  in

  (* Initializing Signals (Initial block) *)
  inputs.rst := Bits.vhigh;
  inputs.link_en := Bits.vlow;
  inputs.tx_write := Bits.vlow;
  
  (* Reset Sequence (#100) *)
  for i = 0 to 10 do cycle () done;
  inputs.rst := Bits.vlow;
  
  (* Enable Link *)
  inputs.link_en := Bits.vhigh;

  (* Wait for Active (Wait loop) *)
  while Bits.is_low !(outputs.link_active) do
    cycle ()
  done;
  Stdio.print_endline "Link is Active. Starting transmission...";

  (* Send Data 0xA5 *)
  if Bits.is_high !(outputs.tx_ready) then begin
    inputs.tx_data := Bits.of_int ~width:9 0xA5;
    inputs.tx_write := Bits.vhigh;
    cycle ();
    inputs.tx_write := Bits.vlow;
  end;

  (* Send EOP 0x100 *)
  inputs.tx_data := Bits.of_int ~width:9 0x100;
  inputs.tx_write := Bits.vhigh;
  cycle ();
  inputs.tx_write := Bits.vlow;

  (* Monitor Reception *)
  for i = 0 to 100 do
    cycle ();
    if Bits.is_high !(outputs.rx_valid) && Bits.to_int !(outputs.rx_data) = 0xA5 then
      Stdio.print_endline "Success: Received Data 0xA5";
  done
