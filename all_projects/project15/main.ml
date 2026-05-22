(** Main — emit RTL and print design statistics.

    Run:  dune exec bin/main.exe -- [verilog|rtlil|stats]

    Output files:
      spw_top.v          — synthesisable Verilog for Gowin EDA / yosys
      spw_spi_slave.v    — standalone SPI slave (useful for verification)
      spw_ds_encoder.v   — DS encoder
      spw_ds_decoder.v   — DS decoder
      spw_link_fsm.v     — link state machine
*)

open Core
open Hardcaml
open Hardcaml_spw

let emit_verilog name circuit =
  let filename = name ^ ".v" in
  let oc = Out_channel.create filename in
  Rtl.output ~output_mode:(Rtl.Output_mode.To_channel oc) Verilog circuit;
  Out_channel.close oc;
  printf "Wrote %s\n" filename

let print_stats name circuit =
  let s = Circuit.signal_graph circuit in
  let n_regs = Signal_graph.filter s ~f:Signal.is_reg |> List.length in
  let n_wires = Signal_graph.filter s ~f:Signal.is_wire |> List.length in
  printf "%-20s  regs=%-4d  wires=%d\n" name n_regs n_wires

let () =
  let mode = if Array.length Sys.argv > 1 then Sys.argv.(1) else "verilog" in
  printf "=== HardCaml SpaceWire + SPI — RTL Generator ===\n\n";

  let circuits =
    [ "spw_ds_encoder", Spw_ds_encoder.circuit ()
    ; "spw_ds_decoder", Spw_ds_decoder.circuit ()
    ; "spw_char_tx",    Spw_char_tx.circuit ()
    ; "spw_char_rx",    Spw_char_rx.circuit ()
    ; "spw_link_fsm",   Spw_link_fsm.circuit ()
    ; "spw_spi_slave",  Spw_spi_slave.circuit ()
    ; "spw_regfile",    Spw_regfile.circuit ()
    ; "spw_top",        Spw_top.circuit ()
    ]
  in

  (match mode with
  | "verilog" | "v" ->
    List.iter circuits ~f:(fun (name, c) -> emit_verilog name c)
  | "stats" ->
    printf "%-20s  %-8s  %s\n" "Module" "regs" "wires";
    printf "%s\n" (String.make 45 '-');
    List.iter circuits ~f:(fun (name, c) -> print_stats name c)
  | _ ->
    eprintf "Usage: main.exe [verilog|stats]\n";
    exit 1);

  printf "\nDone.\n"
