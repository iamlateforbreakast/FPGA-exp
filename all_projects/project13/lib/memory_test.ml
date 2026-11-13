open Hardcaml
open Signal
open Base

(** A small read/write test sequencer shared by the SDRAM and HyperRAM
    controllers.  It writes a unique, address-dependent word to every
    address in [0 .. num_words-1], then reads them all back, then repeats
    with the pattern inverted, and latches [pass] or [fail].  Writing everything before reading anything means an
    address-decoding fault (aliasing) shows up as a mismatch.

    The controller interface is a simple command/complete handshake:
    [read]/[write] are held until [ready] drops, and [done] pulses when the
    operation (including any refresh/precharge delays) has finished. *)
module type C = sig
  val data_width : int
  val addr_width : int
  val num_words : int
  val is_simulation : bool
end

module Make (C : C) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; ram_ready : 'a
      ; ram_done : 'a
      ; ram_dout : 'a [@bits C.data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { read : 'a
      ; write : 'a
      ; addr : 'a [@bits C.addr_width]
      ; din : 'a [@bits C.data_width]
      ; pass : 'a
      ; fail : 'a
      ; active : 'a
      ; heartbeat : 'a
      ; state : 'a [@bits 4]  (** Current FSM state index, for debug *)
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | WAIT_INIT
      | WRITE_READY
      | WRITE
      | WAIT_WRITE
      | READ_READY
      | READ
      | WAIT_READ
      | PASS
      | FAIL
    [@@deriving sexp_of, compare, enumerate]
  end

  let pattern =
    let mask = (1 lsl C.data_width) - 1 in
    if C.data_width >= 32 then 0xA5A5A5A5 land mask else 0x5A5A land mask
  ;;

  let create _scope (i : Signal.t I.t) : Signal.t O.t =
    let open Always in
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in
    let sm = State_machine.create (module States) spec ~enable:vdd in
    let addr = Variable.reg spec ~width:C.addr_width in
    let pass = Variable.reg spec ~width:1 in
    let fail = Variable.reg spec ~width:1 in
    let heartbeat = Variable.reg spec ~width:24 in
    (* Two rounds, the second with the pattern inverted, so that data left
       in the memory by the first round (or by an earlier run -- the RAM
       keeps its contents across FPGA reloads) cannot pass for a write that
       did not happen. *)
    let round = Variable.reg spec ~width:1 in
    let expected =
      uresize addr.value C.data_width
      ^: mux2
           round.value
           (of_int ~width:C.data_width (lnot pattern))
           (of_int ~width:C.data_width pattern)
    in
    let last_addr = of_int ~width:C.addr_width (C.num_words - 1) in
    let match_ = i.ram_dout ==: expected in
    let () =
      compile
        [ heartbeat <-- (heartbeat.value +:. 1)
        ; sm.switch
            [ ( WAIT_INIT
              , [ if_ i.ram_ready [ sm.set_next WRITE_READY ] [] ] )
            ; ( WRITE_READY
              , [ if_ i.ram_ready [ sm.set_next WRITE ] [] ] )
            ; ( WRITE
              , [ if_ (~:(i.ram_ready)) [ sm.set_next WAIT_WRITE ] [] ] )
            ; ( WAIT_WRITE
              , [ if_ i.ram_done
                    [ if_ (addr.value ==: last_addr)
                        [ addr <--. 0; sm.set_next READ_READY ]
                        [ addr <-- (addr.value +:. 1); sm.set_next WRITE_READY ]
                    ]
                    []
                ] )
            ; ( READ_READY
              , [ if_ i.ram_ready [ sm.set_next READ ] [] ] )
            ; ( READ
              , [ if_ (~:(i.ram_ready)) [ sm.set_next WAIT_READ ] [] ] )
            ; ( WAIT_READ
              , [ if_ i.ram_done
                    [ if_ match_
                        [ if_ (addr.value ==: last_addr)
                            [ if_ round.value
                                [ pass <--. 1; sm.set_next PASS ]
                                [ round <--. 1; addr <--. 0; sm.set_next WRITE_READY ]
                            ]
                            [ addr <-- (addr.value +:. 1); sm.set_next READ_READY ]
                        ]
                        [ fail <--. 1; sm.set_next FAIL ]
                    ]
                    []
                ]
              )
            ; PASS, [ sm.set_next PASS ]
            ; FAIL, [ sm.set_next FAIL ]
            ]
        ]
    in
    { O.read = sm.is READ
    ; write = sm.is WRITE
    ; addr = addr.value
    ; din = expected
    ; pass = pass.value
    ; fail = fail.value
    ; active = ~: (sm.is PASS |: sm.is FAIL)
    ; heartbeat = msb heartbeat.value
    ; state = uresize sm.current 4
    }
  ;;
  let hierarchical scope (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory_test" ~instance:"inst_test" create i
;;

end
