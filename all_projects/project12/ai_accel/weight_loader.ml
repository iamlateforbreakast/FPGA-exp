(** Weight loader — reads INT8 binary blobs produced by train.py
    and generates HardCaml ROM / BRAM initialisation constants.

    Usage:
      let weights = Weight_loader.load_all "path/to/weights/" in
      ...
*)

open Hardcaml
open Signal

(* -------------------------------------------------------------------------
   Types
   ------------------------------------------------------------------------- *)

type weight_tensor =
  { name   : string
  ; shape  : int list
  ; scale  : float          (* dequantisation scale *)
  ; data   : int array      (* flat INT8 values, stored as OCaml int *)
  }

(* -------------------------------------------------------------------------
   JSON metadata parser (minimal — no external deps)
   Parses the meta.json produced by train.py.
   ------------------------------------------------------------------------- *)

(* Very small JSON value type — enough for our metadata *)
type json =
  | JString of string
  | JFloat  of float
  | JInt    of int
  | JArray  of json list
  | JObject of (string * json) list

exception Json_error of string

let rec skip_ws s i =
  if !i < String.length s && (s.[!i] = ' ' || s.[!i] = '\n' || s.[!i] = '\r' || s.[!i] = '\t')
  then (incr i; skip_ws s i)

let parse_string s i =
  assert (s.[!i] = '"'); incr i;
  let buf = Buffer.create 16 in
  while s.[!i] <> '"' do
    Buffer.add_char buf s.[!i]; incr i
  done;
  incr i;
  JString (Buffer.contents buf)

let parse_number s i =
  let start = !i in
  if s.[!i] = '-' then incr i;
  while !i < String.length s &&
        (s.[!i] >= '0' && s.[!i] <= '9' || s.[!i] = '.' || s.[!i] = 'e' || s.[!i] = 'E' || s.[!i] = '+' || s.[!i] = '-') do
    incr i
  done;
  let tok = String.sub s start (!i - start) in
  if String.contains tok '.' || String.contains tok 'e'
  then JFloat (float_of_string tok)
  else JInt   (int_of_string tok)

let rec parse_value s i =
  skip_ws s i;
  match s.[!i] with
  | '"'  -> parse_string s i
  | '['  -> parse_array  s i
  | '{'  -> parse_object s i
  | _    -> parse_number s i

and parse_array s i =
  assert (s.[!i] = '['); incr i;
  skip_ws s i;
  if s.[!i] = ']' then (incr i; JArray [])
  else begin
    let items = ref [] in
    let go = ref true in
    while !go do
      items := parse_value s i :: !items;
      skip_ws s i;
      if s.[!i] = ',' then incr i
      else go := false
    done;
    assert (s.[!i] = ']'); incr i;
    JArray (List.rev !items)
  end

and parse_object s i =
  assert (s.[!i] = '{'); incr i;
  skip_ws s i;
  if s.[!i] = '}' then (incr i; JObject [])
  else begin
    let pairs = ref [] in
    let go    = ref true in
    while !go do
      skip_ws s i;
      let key = match parse_string s i with JString k -> k | _ -> assert false in
      skip_ws s i; assert (s.[!i] = ':'); incr i;
      let v   = parse_value s i in
      pairs := (key, v) :: !pairs;
      skip_ws s i;
      if s.[!i] = ',' then incr i
      else go := false
    done;
    assert (s.[!i] = '}'); incr i;
    JObject (List.rev !pairs)
  end

let parse_json str =
  let i = ref 0 in
  parse_value str i

let read_file path =
  let ic = open_in path in
  let n  = in_channel_length ic in
  let s  = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(* -------------------------------------------------------------------------
   Load weights
   ------------------------------------------------------------------------- *)

let load_all weights_dir =
  let meta_str = read_file (Filename.concat weights_dir "meta.json") in
  let meta     = parse_json meta_str in
  match meta with
  | JObject entries ->
    List.map (fun (name, info) ->
      match info with
      | JObject fields ->
        let shape = match List.assoc "shape" fields with
          | JArray dims -> List.map (function JInt n -> n | _ -> assert false) dims
          | _ -> raise (Json_error "bad shape") in
        let scale = match List.assoc "scale" fields with
          | JFloat f -> f | JInt i -> float_of_int i | _ -> assert false in
        let total = List.fold_left ( * ) 1 shape in
        let path  = Filename.concat weights_dir (name ^ ".bin") in
        let ic    = open_in_bin path in
        let buf   = Bytes.create total in
        really_input ic buf 0 total;
        close_in ic;
        let data  = Array.init total (fun k ->
          (* Read as signed int8 *)
          let byte = Char.code (Bytes.get buf k) in
          if byte >= 128 then byte - 256 else byte) in
        { name; shape; scale; data }
      | _ -> raise (Json_error ("bad entry: " ^ name))
    ) entries
  | _ -> raise (Json_error "top level not an object")

(* -------------------------------------------------------------------------
   Convert a flat weight tensor to HardCaml ROM signal list
   Each entry becomes an 8-bit Signal.t constant.
   ------------------------------------------------------------------------- *)

let to_rom_signals (w : weight_tensor) : Signal.t list =
  Array.to_list (Array.map (fun v ->
    (* Re-encode as unsigned 8-bit for Signal.of_int *)
    let uv = if v < 0 then v + 256 else v in
    Signal.of_int ~width:8 uv
  ) w.data)

(* -------------------------------------------------------------------------
   Build a synchronous ROM from a weight tensor row
   row_idx: which row to address (e.g. which hidden unit's weights)
   col_addr: Signal.t address into that row
   ------------------------------------------------------------------------- *)

let make_weight_rom ~clock ~(w : weight_tensor) ~row_addr ~col_addr =
  match w.shape with
  | [rows; cols] ->
    let flat = to_rom_signals w in
    (* 2D address = row_addr * cols + col_addr *)
    let addr =
      (uresize row_addr (Int.of_float (Float.log (float_of_int rows) /. Float.log 2.0) + 1)
       *: Signal.of_int ~width:16 cols)
      +: uresize col_addr 16
    in
    let q = reg (Reg_spec.create ~clock ()) ~enable:vdd (mux addr flat) in
    q
  | _ -> failwith ("make_weight_rom: expected 2D tensor, got " ^ w.name)

(* -------------------------------------------------------------------------
   Vocabulary helpers
   ------------------------------------------------------------------------- *)

type vocab =
  { char2idx  : (char, int) Hashtbl.t
  ; idx2char  : (int, char) Hashtbl.t
  ; vocab_size : int
  ; hidden_size: int
  }

let load_vocab weights_dir =
  let str  = read_file (Filename.concat weights_dir "vocab.json") in
  let json = parse_json str in
  match json with
  | JObject fields ->
    let vocab_size  = (match List.assoc "vocab_size"  fields with JInt n -> n | _ -> assert false) in
    let hidden_size = (match List.assoc "hidden_size" fields with JInt n -> n | _ -> assert false) in
    let char2idx = Hashtbl.create vocab_size in
    let idx2char = Hashtbl.create vocab_size in
    (match List.assoc "char2idx" fields with
     | JObject pairs ->
       List.iter (fun (c_str, idx) ->
         let c   = c_str.[0] in
         let idx = match idx with JInt n -> n | _ -> assert false in
         Hashtbl.add char2idx c   idx;
         Hashtbl.add idx2char idx c
       ) pairs
     | _ -> assert false);
    { char2idx; idx2char; vocab_size; hidden_size }
  | _ -> failwith "load_vocab: bad json"

(* -------------------------------------------------------------------------
   Self-test
   ------------------------------------------------------------------------- *)

let () =
  let dir = Sys.argv.(1) in
  Printf.printf "Loading weights from: %s\n%!" dir;
  let ws = load_all dir in
  List.iter (fun w ->
    let total = Array.length w.data in
    let amax  = Array.fold_left (fun m v -> max m (abs v)) 0 w.data in
    Printf.printf "  %-30s  shape=%-20s  scale=%.6f  |max|=%d\n%!"
      w.name
      (String.concat "x" (List.map string_of_int w.shape))
      w.scale amax
  ) ws;
  let vocab = load_vocab dir in
  Printf.printf "\nVocab size: %d  Hidden: %d\n%!" vocab.vocab_size vocab.hidden_size;
  Printf.printf "Char 'A' -> index %d\n%!" (Hashtbl.find vocab.char2idx 'A');
  Printf.printf "Index 0  -> char '%c'\n%!" (Hashtbl.find vocab.idx2char 0)
