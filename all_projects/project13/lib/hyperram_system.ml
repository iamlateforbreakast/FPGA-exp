open Hardcaml
open Signal

(** HyperRAM controller plus the read/write test sequencer, with no board
    I/O.  The DQ and RWDS pads are wrapped in IOBUFs by the top level. *)
module Make (X : Config_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; dq_in : 'a [@bits 8]
      ; rwds_in : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { led : 'a [@bits X.led_width]
      ; dout : 'a [@bits 16]
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
      ; pass : 'a
      ; fail : 'a
      ; active : 'a
      ; test_state : 'a [@bits 4]
      ; test_addr : 'a [@bits 22]
      }
    [@@deriving hardcaml]
  end

  module Cfg = struct
    let data_width = 16
    let addr_width = 22
    let num_words = 4096
    let is_simulation = X.is_simulation
  end

  module Test = Memory_test.Make (Cfg)
  module Ctrl = Hyperram.Make (X)

  let create scope (i : Signal.t I.t) : Signal.t O.t =
    let read_w = Signal.wire 1 in
    let write_w = Signal.wire 1 in
    let addr_w = Signal.wire 22 in
    let din_w = Signal.wire 16 in
    let ctrl =
      Ctrl.hierarchical
        scope
        { Ctrl.I.clock = i.clock
        ; reset = i.reset
        ; read = read_w
        ; write = write_w
        ; addr = addr_w
        ; din = din_w
        ; dq_in = i.dq_in
        ; rwds_in = i.rwds_in
        }
    in
    let test =
      Test.hierarchical
        scope
        { Test.I.clock = i.clock
        ; reset = i.reset
        ; ram_ready = ctrl.ready
        ; ram_done = ctrl.done_
        ; ram_dout = ctrl.dout
        }
    in
    let () =
      read_w <== test.read;
      write_w <== test.write;
      addr_w <== test.addr;
      din_w <== test.din
    in
    (* The 4K has a single LED, so encode the outcomes (at 81 MHz):
       - solid on: pass
       - still running (hung): mostly on, with N short dark gaps then a long
         on period, repeating.  N is the test FSM state + 1 (1 = WAIT_INIT,
         2 = WRITE_READY, 3 = WRITE, 4 = WAIT_WRITE, 5 = READ_READY,
         6 = READ, 7 = WAIT_READ).
       - on a mismatch, N slow blinks then a pause, repeating.  The test stops
         at the first bad word with [addr] and [dout] still holding it, so
         N = k + 4 when the word read back is the one expected at addr + k
         (k in -3..3, i.e. N = 1..7), and N = 8 when it matches none of them. *)
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in
    let blink = reg_fb spec ~enable:vdd ~width:25 ~f:(fun c -> c +:. 1) in
    let slot_tick = blink ==:. (1 lsl 25) - 1 in
    let slot =
      reg_fb spec ~enable:slot_tick ~width:4 ~f:(fun s ->
        mux2 (s ==:. 11) (zero 4) (s +:. 1))
    in
    let diff =
      (ctrl.dout ^: of_int ~width:16 Test.pattern) -: select test.addr 15 0
    in
    let near = diff +:. 3 <:. 7 in
    let code = mux2 near (uresize (select (diff +:. 4) 2 0) 4) (of_int ~width:4 8) in
    let code_led = (slot <: code) &: ~:(msb blink) in
    let hung_led = ~:((slot <: test.state +:. 1) &: ~:(msb blink)) in
    let led =
      X.normalize_led
        (mux2 test.pass vdd (mux2 test.fail code_led hung_led))
    in
    { O.led
    ; dout = ctrl.dout
    ; done_ = ctrl.done_
    ; ready = ctrl.ready
    ; busy = ctrl.busy
    ; dq_out = ctrl.dq_out
    ; dq_oe = ctrl.dq_oe
    ; rwds_out = ctrl.rwds_out
    ; rwds_oe = ctrl.rwds_oe
    ; hpram_ck = ctrl.hpram_ck
    ; hpram_cs_n = ctrl.hpram_cs_n
    ; hpram_reset_n = ctrl.hpram_reset_n
    ; pass = test.pass
    ; fail = test.fail
    ; active = test.active
    ; test_state = test.state
    ; test_addr = test.addr
    }
  ;;
  let hierarchical scope (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"hyperram_system" ~instance:"inst_hyperram_system" create i
;;

end
