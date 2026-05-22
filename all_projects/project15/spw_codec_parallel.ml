open Base
open Hardcaml
open Hardcaml_step_testbench

(* We use the Spw_interface defined previously *)
module State = struct
  type t = unit (* We don't need global state for this simple test *)
end

let testbench (sim : _ Cyclesim.t) =
  let open Runner.Open_on_command (State) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  (* Helper to wait for a signal condition *)
  let wait_until (signal : Bits.t ref) =
    let rec loop () =
      if Bits.is_high !signal then return ()
      else cycle 1 >>= loop
    in
    loop ()
  in

  (* --- Transmit Process --- *)
  let transmit_process =
    (* Reset Sequence *)
    inputs.rst := Bits.vhigh;
    cycle 10 >>= fun () ->
    inputs.rst := Bits.vlow;
    cycle 5 >>= fun () ->
    
    (* Enable Link *)
    inputs.link_en := Bits.vhigh;
    
    (* Wait for Link Active (wait(link_active)) *)
    wait_until outputs.link_active >>= fun () ->
    Stdio.print_endline "Link is Active. Starting transmission...";

    (* Send Data Character 0xA5 *)
    wait_until outputs.tx_ready >>= fun () ->
    inputs.tx_data := Bits.of_int ~width:9 0xA5;
    inputs.tx_write := Bits.vhigh;
    cycle 1 >>= fun () ->
    inputs.tx_write := Bits.vlow;

    (* Send EOP 0x100 *)
    wait_until outputs.tx_ready >>= fun () ->
    inputs.tx_data := Bits.of_int ~width:9 0x100;
    inputs.tx_write := Bits.vhigh;
    cycle 1 >>= fun () ->
    inputs.tx_write := Bits.vlow;
    return ()
  in

  (* --- Receive/Monitor Process (fork block) --- *)
  let monitor_process =
    let rec check_rx timeout =
      if timeout <= 0 then 
        (Stdio.print_endline "Error: Timeout waiting for data"; return ())
      else if Bits.is_high !(outputs.rx_valid) && Bits.to_int !(outputs.rx_data) = 0xA5 then
        (Stdio.print_endline "Success: Received Data 0xA5"; return ())
      else
        cycle 1 >>= fun () -> check_rx (timeout - 1)
    in
    check_rx 500
  in

  (* Combine processes using spawn to mimic Verilog 'fork' *)
  spawn transmit_process >>= fun _ ->
  spawn monitor_process >>= fun _ ->
  return ()
