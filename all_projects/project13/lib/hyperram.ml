open Hardcaml
open Signal
open Base

(** HyperRAM controller for the 64 Mbit W956x8MKY (8-bit) HyperRAM embedded in
    the Tang Nano 4K's GW1NS-4.

    The protocol is the standard HyperBus register/memory access used by the
    well-known Tang Nano 9K PSRAM controller (zf3/psram-tang-nano-9k), but the
    DDR interface here is driven from a four-phase fabric clock rather than
    ODDR/IDDR primitives.  The fabric clock is 4x the HyperRAM CK, and one
    CK clock of a transaction runs from phase 1 to the following phase 0:

      phase:     1    2    3    0
      CK:        0    1    1    0      (registered, so glitch-free)
      DQ out:   rising    falling      (CK edges centred in each byte)
      DQ in:              ^rise     ^fall (captured at the end of phase 3
                                      and of the next phase 1)

    [dq_sr] shifts 16 bits per CK clock, at the end of phase 0.  Driving CK
    one phase after the data puts each CK edge in the middle of its byte;
    an edge-aligned CK let the memory latch half-changed CA bytes on
    hardware.  The capture points allow for the pad round trip (CK out,
    tCKD, DQ back in), which is about one fabric clock on the Nano 4K:
    read data reaches the fabric roughly a phase after the CK edge that
    launched it, so the rising byte is captured at the end of phase 3 and
    the falling byte at the end of the next phase 1.  The pair is then
    assembled as {rising_byte, falling_byte}, like the reference
    controller's IDDR.
    The CR0 write at startup selects variable latency with 3 initial clocks,
    which is the reference configuration for 81 MHz. *)
module Make (X : Config_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; read : 'a
      ; write : 'a
      ; addr : 'a [@bits 22]  (** 16-bit word address (8 MiB / 2 bytes) *)
      ; din : 'a [@bits 16]
      ; dq_in : 'a [@bits 8]
      ; rwds_in : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { dout : 'a [@bits 16]
      ; done_ : 'a [@rtlname "done"]
      ; ready : 'a
      ; busy : 'a
      ; dq_out : 'a [@bits 8]
      ; dq_oe : 'a
      ; rwds_out : 'a
      ; rwds_oe : 'a
      ; hpram_ck : 'a [@rtlname "O_hpram_ck"]
      ; hpram_cs_n : 'a [@rtlname "O_hpram_cs_n"]
      ; hpram_reset_n : 'a [@rtlname "O_hpram_reset_n"]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = INIT | CONFIG | CONFIG_WAIT | IDLE | READ | WRITE | GAP
    [@@deriving sexp_of, compare, enumerate]
  end

  (* 160 us power-on delay, in fabric cycles. *)
  let init_cycles = X.pll_freq * 160 / 1_000_000

  (* 3 initial latency clocks, variable 1x/2x read latency.  This is the
     value used by the reference controller for 81 MHz. *)
  let latency = 3
  let cr_latency = 0xE
  let config64 =
    Stdlib.Int64.logor
      (Stdlib.Int64.logor
         0x6000010000008F00L
         (Stdlib.Int64.shift_left (Stdlib.Int64.of_int cr_latency) 4))
      0x7L


  let create _scope (i : Signal.t I.t) : Signal.t O.t =
    let open Always in
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in
    let sm = State_machine.create (module States) spec ~enable:vdd in
    let phase = Variable.reg spec ~width:2 in
    let cycles_sr = Variable.reg spec ~width:24 in
    let dq_sr = Variable.reg spec ~width:64 in
    let rwds_out_ris = Variable.reg spec ~width:1 in
    let rwds_out_fal = Variable.reg spec ~width:1 in
    let dq_oe_r = Variable.reg spec ~width:1 in
    let rwds_oe_r = Variable.reg spec ~width:1 in
    let ck_en_r = Variable.reg spec ~width:1 in
    let wait_rd = Variable.reg spec ~width:1 in
    let additional_latency = Variable.reg spec ~width:1 in
    let w_din = Variable.reg spec ~width:16 in
    let dout_r = Variable.reg spec ~width:16 in
    let done_r = Variable.reg spec ~width:1 in
    let init_cnt = Variable.reg spec ~width:15 in
    let init_done = Variable.reg spec ~width:1 in
    let is_write = Variable.reg spec ~width:1 in
    let addr_r = Variable.reg spec ~width:22 in
    let dq_in_ris = Variable.reg spec ~width:8 in
    let dq_in_fal = Variable.reg spec ~width:8 in
    let rwds_in_ris = Variable.reg spec ~width:1 in
    let rwds_in_fal = Variable.reg spec ~width:1 in
    (* [read_done] hands the phase-2 read capture to the phase-0 state
       machine. *)
    let read_done = Variable.reg spec ~width:1 in
    let gap_cnt = Variable.reg spec ~width:2 in
    (* Output-timing registers.  [hi_sel] is high in phases 1-2 (the rising
       byte) and [ck_r] in phases 2-3, both computed one fabric clock ahead
       so that neither the DQ byte select nor CK is decoded from the phase
       counter combinationally. *)
    let hi_sel = Variable.reg spec ~width:1 in
    let ck_r = Variable.reg spec ~width:1 in
    let ce = phase.value ==:. 0 in
    let is = sm.is in
    let dq_out =
      mux2 hi_sel.value (select dq_sr.value 63 56) (select dq_sr.value 55 48)
    in
    let rwds_out =
      mux2 hi_sel.value rwds_out_ris.value rwds_out_fal.value
    in
    let hpram_ck = ck_r.value in
    let ready = is IDLE &: init_done.value in
    (* 48-bit HyperBus CA: RW, AS=0, burst type=1, then the 29-bit address
       field CA[44:16], 13 reserved zero bits and the lower 3 address bits.
       With a 22-bit 16-bit-word address the byte address is word_addr << 1,
       so CA[34:16] = word_addr[21:3] and CA[2:0] = word_addr[2:0]. *)
    let ca_for ~rw =
      concat_msb
        [ rw
        ; gnd (* AS = 0: memory access *)
        ; vdd (* linear burst *)
        ; of_int ~width:10 0
        ; select i.addr 21 3
        ; of_int ~width:13 0
        ; select i.addr 2 0
        ]
    in
    let ca_start = concat_msb [ ca_for ~rw:(~:(i.write)); i.din ] in
    let dq_shifted = sll dq_sr.value 16 in
    (* cycles_sr bit k is set during CK clock k of the transaction (CA =
       clocks 1..3), and a decision taken at bit k takes effect in clock
       k + 1.  The latency count starts at CA clock 3, so memory data starts
       in clock 3 + latency (1x) or 3 + 2 * latency (2x) -- measured on the
       Nano 4K with the debug trace.  Write data is therefore loaded at bit
       2 + latency or bit 2 + 2 * latency. *)
    let write_trigger =
      (select cycles_sr.value (2 + latency) (2 + latency)
      &: ~:(additional_latency.value))
      |: select cycles_sr.value (2 + (latency * 2)) (2 + (latency * 2))
    in
    let () =
      compile
        [ phase <-- (phase.value +:. 1)
        ; hi_sel <-- ((phase.value ==:. 0) |: (phase.value ==:. 1))
        ; ck_r <-- (ck_en_r.value &: ((phase.value ==:. 1) |: (phase.value ==:. 2)))
        ; if_ (init_cnt.value <: of_int ~width:15 init_cycles)
            [ init_cnt <-- (init_cnt.value +:. 1) ]
            []
        ; (* DDR input capture, see the timing diagram above. *)
          when_ (phase.value ==:. 3)
            [ dq_in_ris <-- i.dq_in; rwds_in_ris <-- i.rwds_in ]
        ; when_ (phase.value ==:. 1)
            [ dq_in_fal <-- i.dq_in; rwds_in_fal <-- i.rwds_in ]
        ; (* In phase 2 of CK clock k + 1 both bytes of clock k are held:
             the rising byte from phase 3 of clock k and the falling byte
             from phase 1 of clock k + 1.  Capture the first word there; the
             phase-0 FSM then stops CK and moves to GAP. *)
          when_
            ((phase.value ==:. 2)
            &: (is READ)
            &: wait_rd.value
            &: ~:(read_done.value)
            &: (rwds_in_ris.value ^: rwds_in_fal.value))
            [ dout_r <-- concat_msb [ dq_in_ris.value; dq_in_fal.value ]
            ; done_r <--. 1
            ; read_done <--. 1
            ]
        ; when_ ce
            [ done_r <--. 0;
              cycles_sr <-- sll cycles_sr.value 1;
              dq_sr <-- dq_shifted;
              sm.switch
                [ ( INIT
                  , [ if_
                        (init_cnt.value >=: of_int ~width:15 init_cycles)
                        [ cycles_sr <--. 2
                        ; dq_sr <-- of_int64 ~width:64 config64
                        ; dq_oe_r <--. 1
                        ; ck_en_r <--. 1
                        ; sm.set_next CONFIG
                        ]
                        []
                    ] )
                ; ( CONFIG, [ sm.set_next CONFIG_WAIT ] )
                ; ( CONFIG_WAIT
                  , [ if_ (select cycles_sr.value 4 4)
                        [ ck_en_r <--. 0
                        ; dq_oe_r <--. 0
                        ; cycles_sr <--. 2
                        ; init_done <--. 1
                        ; gap_cnt <--. 0
                        ; sm.set_next GAP
                        ]
                        []
                    ] )
                ; ( IDLE
                  , [ ck_en_r <--. 0
                    ; dq_oe_r <--. 0
                    ; rwds_oe_r <--. 0
                    ; wait_rd <--. 0
                    ; if_ (i.read |: i.write)
                        [ addr_r <-- i.addr
                        ; is_write <-- i.write
                        ; dq_sr <-- ca_start
                        ; cycles_sr <--. 2
                        ; ck_en_r <--. 1
                        ; dq_oe_r <--. 1
                        ; rwds_oe_r <--. 0
                        ; wait_rd <--. 0
                        ; w_din <-- i.din
                        ; if_ i.write [ sm.set_next WRITE ] [ sm.set_next READ ]
                        ]
                        []
                    ] )
                ; ( READ
                  , [ if_ (select cycles_sr.value 3 3) [ dq_oe_r <--. 0 ] []
                    ; (* Watch for RWDS toggling from clock 3 + latency, the
                         earliest (1x) data clock: [wait_rd] set at bit k is
                         first checked in clock k + 1 against the RWDS pair
                         sampled in clock k.  RWDS stays low through any
                         extra 2x latency, so the first toggle is always the
                         first word of the burst. *)
                      if_ (select cycles_sr.value (3 + latency) (3 + latency))
                        [ wait_rd <--. 1 ]
                        []
                    ; if_ read_done.value
                        [ read_done <--. 0
                        ; ck_en_r <--. 0
                        ; gap_cnt <--. 0
                        ; sm.set_next GAP
                        ]
                        []
                    ] )
                ; ( WRITE
                  , [ (* The memory drives RWDS high during the CA to ask for
                         2x latency.  At bit 3 [rwds_in_fal] holds the
                         phase-1 sample from CK clock 3, still inside the
                         CA. *)
                      if_ (select cycles_sr.value 3 3)
                        [ additional_latency <-- rwds_in_fal.value ]
                        []
                    ; if_ write_trigger
                        [ rwds_oe_r <--. 1
                        ; dq_sr <-- concat_msb [ w_din.value; select dq_shifted 47 0 ]
                        ; rwds_out_ris <--. 0
                        ; rwds_out_fal <--. 0
                        ; done_r <--. 1
                        ; gap_cnt <--. 0
                        ; sm.set_next GAP
                        ]
                        []
                    ] )
                ; ( GAP
                  , [ ck_en_r <--. 0
                    ; dq_oe_r <--. 0
                    ; rwds_oe_r <--. 0
                    ; if_ (gap_cnt.value ==:. 3)
                        [ sm.set_next IDLE ]
                        [ gap_cnt <-- (gap_cnt.value +:. 1) ]
                    ] )
                ]
            ]
        ]
    in
    { O.dout = dout_r.value
    ; done_ = done_r.value
    ; ready
    ; busy = ~: ready
    ; dq_out
    ; dq_oe = dq_oe_r.value
    ; rwds_out
    ; rwds_oe = rwds_oe_r.value
    ; hpram_ck
    ; hpram_cs_n = ~:(ck_en_r.value)
    ; hpram_reset_n = ~:(i.reset)
    }
  ;;
  let hierarchical scope (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"hyperram" ~instance:"inst_hyperram" create i
;;

end
