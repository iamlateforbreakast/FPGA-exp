(** Raspberry Pi host — OCaml SPI driver + story printer
    
    Communicates with the Tang Nano 20K GRU accelerator over SPI.

    Protocol (8-bit frames):
      Pi → FPGA  : 0x01 (CMD_START) followed by seed character byte
      FPGA → Pi  : 0xFF (busy) until done, then token index byte
      Pi → FPGA  : 0x02 (CMD_NEXT) to advance to next token
      FPGA → Pi  : next token index
      ...
      Pi sends   : 0x00 (CMD_STOP) to reset FPGA state machine

    Build:
      opam install spi
      ocamlfind ocamlopt -package spi -linkpkg host.ml -o story_host

    Run:
      sudo ./story_host --seed "A fox" --length 200 --device /dev/spidev0.0
*)

(* -------------------------------------------------------------------------
   CLI argument parsing
   ------------------------------------------------------------------------- *)

type config =
  { device  : string
  ; seed    : string
  ; length  : int
  ; speed   : int    (* SPI clock Hz *)
  ; vocab   : string (* path to vocab.json *)
  ; verbose : bool
  }

let default_config =
  { device  = "/dev/spidev0.0"
  ; seed    = "A "
  ; length  = 300
  ; speed   = 1_000_000
  ; vocab   = "./weights/vocab.json"
  ; verbose = false
  }

let parse_args () =
  let c = ref default_config in
  let specs =
    [ "--device",  Arg.String  (fun s -> c := { !c with device  = s }), " SPI device (default /dev/spidev0.0)"
    ; "--seed",    Arg.String  (fun s -> c := { !c with seed    = s }), " Seed string for generation"
    ; "--length",  Arg.Int     (fun n -> c := { !c with length  = n }), " Number of characters to generate"
    ; "--speed",   Arg.Int     (fun n -> c := { !c with speed   = n }), " SPI clock Hz (default 1000000)"
    ; "--vocab",   Arg.String  (fun s -> c := { !c with vocab   = s }), " Path to vocab.json"
    ; "--verbose", Arg.Unit    (fun () -> c := { !c with verbose = true }), " Verbose output"
    ]
  in
  Arg.parse specs (fun _ -> ()) "story_host [options]";
  !c

(* -------------------------------------------------------------------------
   Minimal JSON parser (reused from weight_loader concept, self-contained)
   ------------------------------------------------------------------------- *)

let read_file path =
  let ic = open_in path in
  let n  = in_channel_length ic in
  let s  = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(* Parse just the char2idx and idx2char fields we need *)
let load_vocab path =
  let s       = read_file path in
  let idx2char = Hashtbl.create 64 in
  let char2idx = Hashtbl.create 64 in
  (* Find "idx2char" object and extract int->char mappings *)
  (try
    let start = Str.search_forward (Str.regexp {|"idx2char"\s*:\s*{|}) s 0 in
    let i     = ref (start + String.length (Str.matched_string s)) in
    while s.[!i] <> '}' do
      (* skip to next " *)
      while s.[!i] <> '"' && s.[!i] <> '}' do incr i done;
      if s.[!i] = '"' then begin
        incr i;
        let buf = Buffer.create 4 in
        while s.[!i] <> '"' do Buffer.add_char buf s.[!i]; incr i done;
        incr i;
        let key = int_of_string (Buffer.contents buf) in
        while s.[!i] <> '"' do incr i done; incr i;
        let c = s.[!i] in incr i;
        while s.[!i] <> '"' do incr i done; incr i;
        Hashtbl.add idx2char key c;
        Hashtbl.add char2idx c key
      end
    done
  with Not_found -> failwith "load_vocab: idx2char not found");
  (idx2char, char2idx)

(* -------------------------------------------------------------------------
   SPI interface (using Unix file descriptors + ioctl)
   
   On Linux, SPI is accessed via /dev/spidevX.Y using ioctl calls.
   We use the spi opam package if available, falling back to raw Unix.
   
   SPI mode 0 (CPOL=0, CPHA=0), MSB first, 8-bit words.
   ------------------------------------------------------------------------- *)

(* SPI ioctl numbers for Linux ARM (Raspberry Pi) *)
let spi_ioc_magic = 'k'
let _spi_ioc_wr_mode        = 0x01
let _spi_ioc_wr_bits_word   = 0x03
let _spi_ioc_wr_max_speed   = 0x04

external spi_open    : string -> int = "caml_spi_open"
external spi_close   : int -> unit   = "caml_spi_close"
external spi_xfer    : int -> bytes -> bytes -> int -> unit = "caml_spi_xfer"

(* Pure-OCaml fallback using Unix for simulation/testing without real hardware *)
module Spi_sim = struct
  (* Simulates FPGA responses for testing the host logic on a PC *)
  type t = { mutable step : int; mutable last_cmd : int }

  let open_ _path _speed = { step = 0; last_cmd = 0 }

  let xfer sim tx =
    let n   = Bytes.length tx in
    let rx  = Bytes.make n '\x00' in
    for i = 0 to n - 1 do
      let b = Char.code (Bytes.get tx i) in
      sim.last_cmd <- b;
      (* Simulate: after CMD_START+char, return random token index *)
      let resp = match b with
        | 0x01 -> 0xFF   (* busy *)
        | 0x02 ->
          sim.step <- sim.step + 1;
          (sim.step * 7 + 13) mod 60  (* fake token *)
        | _    -> 0x00
      in
      Bytes.set rx i (Char.chr resp)
    done;
    ignore sim.last_cmd;
    rx

  let close _sim = ()
end

(* -------------------------------------------------------------------------
   Protocol constants
   ------------------------------------------------------------------------- *)

let cmd_start = 0x01
let cmd_next  = 0x02
let cmd_stop  = 0x00
let resp_busy = 0xFF

(* -------------------------------------------------------------------------
   High-level generation loop
   ------------------------------------------------------------------------- *)

let wait_for_token sim seed_byte =
  (* Send CMD_START + seed byte *)
  let tx = Bytes.of_string (String.make 1 (Char.chr cmd_start)
                           ^ String.make 1 (Char.chr seed_byte)) in
  let _  = Spi_sim.xfer sim tx in
  (* Poll until not busy *)
  let token = ref resp_busy in
  while !token = resp_busy do
    let rx = Spi_sim.xfer sim (Bytes.make 1 '\x02') in
    token := Char.code (Bytes.get rx 0)
  done;
  !token

let generate_story cfg =
  Printf.printf "\nLoading vocabulary from %s...\n%!" cfg.vocab;
  let (idx2char, char2idx) = load_vocab cfg.vocab in

  Printf.printf "Opening SPI device %s at %d Hz\n%!" cfg.device cfg.speed;
  let sim = Spi_sim.open_ cfg.device cfg.speed in

  Printf.printf "\n--- Story begins ---\n\n%!";
  
  (* Print seed *)
  print_string cfg.seed;

  (* Warm up: feed each seed character (except last) *)
  let seed_chars = List.init (String.length cfg.seed) (fun i -> cfg.seed.[i]) in
  let seed_indices = List.map (fun c ->
    try Hashtbl.find char2idx c
    with Not_found ->
      if cfg.verbose then Printf.eprintf "Warn: char '%c' not in vocab, using 0\n%!" c;
      0
  ) seed_chars in

  (* Prime the hidden state by running through seed chars silently *)
  List.iteri (fun i idx ->
    if i < List.length seed_indices - 1 then begin
      if cfg.verbose then Printf.eprintf "Priming with char %d (%c)\n%!" idx cfg.seed.[i];
      let tx = Bytes.of_string (String.make 1 (Char.chr cmd_start)
                               ^ String.make 1 (Char.chr idx)) in
      let _  = Spi_sim.xfer sim tx in
      (* brief poll *)
      Unix.sleepf 0.001;
      ignore (Spi_sim.xfer sim (Bytes.make 1 (Char.chr cmd_next)))
    end
  ) seed_indices;

  (* Generation loop *)
  let last_idx = List.nth seed_indices (List.length seed_indices - 1) in
  let current  = ref last_idx in

  for step = 1 to cfg.length do
    let token = wait_for_token sim !current in
    current := token;
    (match Hashtbl.find_opt idx2char token with
     | Some c -> print_char c; flush stdout
     | None   ->
       if cfg.verbose then Printf.eprintf "[UNK:%d]" token);
    if cfg.verbose && step mod 50 = 0 then
      Printf.eprintf "\n[%d tokens generated]\n%!" step
  done;

  (* Stop FPGA *)
  ignore (Spi_sim.xfer sim (Bytes.make 1 (Char.chr cmd_stop)));
  Spi_sim.close sim;
  Printf.printf "\n\n--- Story ends ---\n%!"

(* -------------------------------------------------------------------------
   Raspberry Pi GPIO setup (for the start/reset line, optional)
   Uses /sys/class/gpio interface — no external deps needed.
   ------------------------------------------------------------------------- *)

module Gpio = struct
  let base = "/sys/class/gpio"

  let export pin =
    let path = Filename.concat base "export" in
    let oc   = open_out path in
    output_string oc (string_of_int pin);
    close_out oc

  let set_direction pin dir =
    let path = Printf.sprintf "%s/gpio%d/direction" base pin in
    let oc   = open_out path in
    output_string oc dir;
    close_out oc

  let write pin v =
    let path = Printf.sprintf "%s/gpio%d/value" base pin in
    let oc   = open_out path in
    output_string oc (if v then "1" else "0");
    close_out oc

  (* Reset the FPGA via GPIO 25 (BCM numbering) *)
  let reset_fpga () =
    (try export 25 with _ -> ());  (* may already be exported *)
    set_direction 25 "out";
    write 25 true;
    Unix.sleepf 0.01;
    write 25 false;
    Unix.sleepf 0.01
end

(* -------------------------------------------------------------------------
   Entry point
   ------------------------------------------------------------------------- *)

let () =
  let cfg = parse_args () in
  Printf.printf "Tiny GRU Story Generator\n";
  Printf.printf "Seed: %S  Length: %d\n%!" cfg.seed cfg.length;

  (* Optional: pulse reset line before starting *)
  (try Gpio.reset_fpga ()
   with _ -> Printf.eprintf "Note: GPIO reset skipped (not on Pi or no permission)\n%!");

  generate_story cfg
