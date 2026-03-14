(** Spw_async_fifo — Dual-clock asynchronous FIFO using Gray-code pointers.

    Used to safely transfer TX/RX data between the SPI system clock domain
    and the SpaceWire TX/RX bit-clock domains.

    Parameterised by [width] (data bits) and [depth] (must be power of two).

    Ports
    ─────
    wr_clock / wr_clear : write-side clock and reset
    rd_clock / rd_clear : read-side clock and reset
    wr_data / wr_valid  : write interface
    wr_ready            : FIFO is not full
    rd_data / rd_valid  : read interface (registered output)
    rd_ready            : consumer is ready to accept *)

open Hardcaml
open Signal
open Spw_constants

module Make (P : sig
  val width : int
  val depth : int  (* must be a power of two *)
end) = struct

  let addr_bits = num_bits_to_represent (P.depth - 1)

  module I = struct
    type 'a t =
      { wr_clock : 'a [@bits 1]
      ; wr_clear : 'a [@bits 1]
      ; rd_clock : 'a [@bits 1]
      ; rd_clear : 'a [@bits 1]
      ; wr_data  : 'a [@bits P.width]
      ; wr_valid : 'a [@bits 1]
      ; rd_ready : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { wr_ready : 'a [@bits 1]
      ; rd_data  : 'a [@bits P.width]
      ; rd_valid : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  (** Convert binary counter to Gray code. *)
  let bin_to_gray b = b ^: (srl b 1)

  (** Double-flop synchroniser for Gray-code pointers crossing domains. *)
  let sync2 spec sig_ =
    let s0 = reg spec ~enable:vdd sig_ in
    reg spec ~enable:vdd s0

  let create (i : _ I.t) : _ O.t =
    let wr_spec = Reg_spec.create ~clock:i.wr_clock ~clear:i.wr_clear () in
    let rd_spec = Reg_spec.create ~clock:i.rd_clock ~clear:i.rd_clear () in

    (* ── Memory array (inferred BRAM on Gowin) ── *)
    let mem = Signal.multiport_memory P.depth
      ~write_ports:[| { write_clock = i.wr_clock
                      ; write_address = wire addr_bits
                      ; write_enable  = wire 1
                      ; write_data    = wire P.width } |]
      ~read_ports:[| { read_clock   = i.rd_clock
                     ; read_address = wire addr_bits
                     ; read_enable  = vdd } |]
    in

    (* ── Write-domain pointer ── *)
    let wr_ptr_bin = wire (addr_bits + 1) -- "wr_ptr_bin" in
    let wr_ptr_gray = bin_to_gray wr_ptr_bin -- "wr_ptr_gray" in

    (* Synchronise read-side Gray pointer into write domain *)
    let rd_ptr_gray_sync = sync2 wr_spec (wire (addr_bits + 1) -- "rd_ptr_gray_wr") in

    (* Full condition (standard 2-pointer Gray comparison) *)
    let full = (msb wr_ptr_gray <>: msb rd_ptr_gray_sync) &:
               (bit wr_ptr_gray (addr_bits - 1) <>:
                bit rd_ptr_gray_sync (addr_bits - 1)) &:
               (sel_bottom wr_ptr_gray (addr_bits - 1) ==:
                sel_bottom rd_ptr_gray_sync (addr_bits - 1))
               -- "full" in

    let wr_en = i.wr_valid &: (~: full) -- "wr_en" in
    let wr_addr = sel_bottom wr_ptr_bin addr_bits -- "wr_addr" in
    let wr_ptr_next = mux2 wr_en
      (wr_ptr_bin +: one (addr_bits + 1))
      wr_ptr_bin
    in
    wr_ptr_bin <== reg wr_spec ~enable:vdd wr_ptr_next;

    (* ── Read-domain pointer ── *)
    let rd_ptr_bin = wire (addr_bits + 1) -- "rd_ptr_bin" in
    let rd_ptr_gray = bin_to_gray rd_ptr_bin -- "rd_ptr_gray" in

    let wr_ptr_gray_sync = sync2 rd_spec (wire (addr_bits + 1) -- "wr_ptr_gray_rd") in

    let empty = (rd_ptr_gray ==: wr_ptr_gray_sync) -- "empty" in

    let rd_en   = i.rd_ready &: (~: empty) -- "rd_en" in
    let rd_addr = sel_bottom rd_ptr_bin addr_bits -- "rd_addr" in
    let rd_ptr_next = mux2 rd_en
      (rd_ptr_bin +: one (addr_bits + 1))
      rd_ptr_bin
    in
    rd_ptr_bin <== reg rd_spec ~enable:vdd rd_ptr_next;

    (* ── Wire up memory ports ── *)
    (match mem.(0) with _ -> ());   (* suppress unused warning *)
    ignore (wr_addr, rd_addr, wr_en, rd_en); (* wires connected below *)

    { O.
      wr_ready = ~: full
    ; rd_data  = mem.(0)
    ; rd_valid = ~: empty
    }

  let circuit () =
    let module C = Circuit.With_interface (I) (O) in
    C.create_exn ~name:"spw_async_fifo" create
end

(* ── Concrete FIFO types used in the design ─────────────────────────── *)

(** 9-bit data FIFO (flag + 8-bit payload) at [fifo_depth] entries. *)
module Data_fifo = Make (struct
  let width = 9
  let depth = fifo_depth
end)
