open Hardcaml
open Signal
open Base

(** On-board logic analyser read out over SPI, for the Nano 4K, whose only
    other output is one LED.

    When [trigger] is first seen, [probe] is recorded on every fabric clock
    into a [trace_depth]-entry buffer.  A host (the project09 Raspberry Pi
    rig) reads it as an SPI mode-0 master: every transfer starts at byte 0
    when CS# falls and returns [header] (16 bytes) followed by the trace,
    each entry as two bytes, most-significant first.  SCLK and CS# are
    oversampled in the fabric clock domain, so keep SCLK at a few MHz at
    most. *)
module Make (C : sig
    val trace_depth : int
  end) =
struct
  let header_bytes = 16
  let addr_bits = Int.ceil_log2 C.trace_depth

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; sclk : 'a
      ; cs_n : 'a
      ; trigger : 'a
      ; probe : 'a [@bits 16]
      ; header : 'a [@bits 8 * header_bytes]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { miso : 'a
      ; trace_done : 'a
      }
    [@@deriving hardcaml]
  end

  let create _scope (i : Signal.t I.t) : Signal.t O.t =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in
    (* Capture. *)
    let recording = Always.Variable.reg spec ~width:1 in
    let trace_done = Always.Variable.reg spec ~width:1 in
    let wr_addr = Always.Variable.reg spec ~width:addr_bits in
    (* The trigger cycle itself is entry 0. *)
    let start = i.trigger &: ~:(trace_done.value) &: ~:(recording.value) in
    (* SPI, with two-flop synchronisers. *)
    let sync x = reg spec (reg spec x) in
    let sclk = sync i.sclk in
    let cs_n = sync (i.cs_n) in
    let sclk_d = reg spec sclk in
    let cs_n_d = reg spec cs_n in
    let sclk_fall = sclk_d &: ~:sclk in
    let cs_fall = cs_n_d &: ~:cs_n in
    let byte_idx = Always.Variable.reg spec ~width:12 in
    let bit_idx = Always.Variable.reg spec ~width:3 in
    let shreg = Always.Variable.reg spec ~width:8 in
    let header_byte idx =
      List.init header_bytes ~f:(fun k ->
        select i.header ((8 * (header_bytes - k)) - 1) (8 * (header_bytes - k - 1)))
      |> mux (select idx 3 0)
    in
    (* The byte after the current one, so the RAM's registered read data
       has settled long before the falling SCLK edge that loads it. *)
    let next_byte = byte_idx.value +:. 1 in
    let trace_byte = next_byte -:. header_bytes in
    let rd_addr = select trace_byte addr_bits 1 in
    let ram =
      Ram.create
        ~collision_mode:Read_before_write
        ~size:C.trace_depth
        ~write_ports:
          [| { write_clock = i.clock
             ; write_address = mux2 start (Signal.zero addr_bits) wr_addr.value
             ; write_enable = recording.value |: start
             ; write_data = i.probe
             }
          |]
        ~read_ports:
          [| { read_clock = i.clock; read_address = rd_addr; read_enable = vdd } |]
        ()
    in
    let entry = ram.(0) in
    let next_value =
      mux2
        (next_byte <:. header_bytes)
        (header_byte next_byte)
        (mux2
           (next_byte >=:. header_bytes + (2 * C.trace_depth))
           (Signal.zero 8)
           (mux2 (lsb trace_byte) (select entry 7 0) (select entry 15 8)))
    in
    let () =
      let open Always in
      compile
        [ if_ recording.value
            [ wr_addr <-- wr_addr.value +:. 1
            ; when_ (wr_addr.value ==:. C.trace_depth - 1)
                [ recording <--. 0; trace_done <--. 1 ]
            ]
            [ when_ start [ recording <--. 1; wr_addr <--. 1 ] ]
        ; if_ cs_fall
            [ byte_idx <--. 0; bit_idx <--. 0; shreg <-- header_byte (Signal.zero 12) ]
            [ when_ (sclk_fall &: ~:cs_n)
                [ bit_idx <-- bit_idx.value +:. 1
                ; if_ (bit_idx.value ==:. 7)
                    [ byte_idx <-- next_byte; shreg <-- next_value ]
                    [ shreg <-- sll shreg.value 1 ]
                ]
            ]
        ]
    in
    { O.miso = msb shreg.value; trace_done = trace_done.value }
  ;;

  let hierarchical scope (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"debug_spi" ~instance:"inst_debug_spi" create i
  ;;
end
