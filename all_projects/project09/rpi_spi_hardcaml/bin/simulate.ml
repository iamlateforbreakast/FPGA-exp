(* simulate.ml *)
open Hardcaml
open Hardcaml_waveterm
open Project09_lib

module Common_config = struct
  let por_bits = 4
  let is_simulation = true
end

module Config_nano20k = struct
  include Common_config
  let normalize_reset x = x
  let led_width = 6
end

module Config_nano4k = struct
  include Common_config
  let normalize_reset x = Signal.(~:x)
  let led_width = 1
end

let testbench (module X : Config.S) ~idle_reset =
  let module MyTop = Top.Make (X) in
  let module Simulator = Cyclesim.With_interface (MyTop.I) (MyTop.O) in
  let scope =
    Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()
  in
  let oc = open_out "rpi_spi.vcd" in
  let sim =
    Simulator.create ~config:Cyclesim.Config.trace_all (MyTop.create scope)
    |> Vcd.wrap oc
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in

  (* Idle levels for SPI mode 0. The reset pin idles at the board-specific
     level: low on the Nano 20K's active-high button, high on the Nano 4K's
     active-low, pulled-up button. *)
  inputs.i_reset := idle_reset;
  inputs.i_cs_n := Bits.vdd;
  inputs.i_sclk := Bits.gnd;
  inputs.i_mosi := Bits.gnd;

  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in

  (* Slow relative to the 27 MHz system clock (four clocks per half period),
     which is representative of a Raspberry Pi running SPI at a few MHz. *)
  let half_period = 4 in

  (* Let the power-on-reset counter fill before touching CS. *)
  cycles (1 lsl Common_config.por_bits);

  let transfer value =
    inputs.i_cs_n := Bits.gnd;
    cycles half_period;
    let miso = ref 0 in
    for bit = 7 downto 0 do
      inputs.i_mosi := Bits.of_int ~width:1 ((value lsr bit) land 1);
      inputs.i_sclk := Bits.vdd;
      cycles half_period;
      (* MISO is valid on the rising edge in mode 0. *)
      miso := (!miso lsl 1) lor Bits.to_int !(outputs.o_miso);
      inputs.i_sclk := Bits.gnd;
      cycles half_period
    done;
    inputs.i_cs_n := Bits.vdd;
    cycles half_period;
    !miso
  in

  List.iter
    (fun value ->
      let received = transfer value in
      Printf.printf "sent 0x%02X  miso 0x%02X  leds 0x%02X\n" value received
        (Bits.to_int !(outputs.leds)))
    [ 0x01; 0x02; 0x2A; 0x41; 0x55; 0xAA ];

  close_out oc;
  waves

let () =
  let board = Option.value (Sys.getenv_opt "BOARD") ~default:"nano20k" in
  let config, idle_reset =
    match board with
    | "nano20k" -> ((module Config_nano20k : Config.S), Bits.gnd)
    | "nano4k" -> ((module Config_nano4k : Config.S), Bits.vdd)
    | other ->
      failwith
        (Printf.sprintf "Unknown BOARD '%s'; expected nano20k or nano4k" other)
  in
  let waves = testbench config ~idle_reset in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves
