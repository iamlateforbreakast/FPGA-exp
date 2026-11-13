open Hardcaml
open Signal

(** SDRAM controller plus the read/write test sequencer, with no board I/O.
    This is what the simulation and the top-level wrapper both instantiate. *)
module Make (X : Config_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; dq_in : 'a [@bits 32]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds : 'a [@bits X.led_width]
      ; dout : 'a [@bits 32]
      ; done_ : 'a [@rtlname "done"]
      ; ready : 'a
      ; busy : 'a
      ; dq_out : 'a [@bits 32]
      ; dq_oe : 'a
      ; sdram_clk : 'a [@rtlname "O_sdram_clk"]
      ; sdram_cke : 'a [@rtlname "O_sdram_cke"]
      ; sdram_cs_n : 'a [@rtlname "O_sdram_cs_n"]
      ; sdram_ras_n : 'a [@rtlname "O_sdram_ras_n"]
      ; sdram_cas_n : 'a [@rtlname "O_sdram_cas_n"]
      ; sdram_wen_n : 'a [@rtlname "O_sdram_wen_n"]
      ; sdram_addr : 'a [@bits 11] [@rtlname "O_sdram_addr"]
      ; sdram_ba : 'a [@bits 2] [@rtlname "O_sdram_ba"]
      ; sdram_dqm : 'a [@bits 4] [@rtlname "O_sdram_dqm"]
      ; pass : 'a
      ; fail : 'a
      ; active : 'a
      }
    [@@deriving hardcaml]
  end

  module Cfg = struct
    let data_width = 32
    let addr_width = 21
    let num_words = 4096
    let is_simulation = X.is_simulation
  end

  module Test = Memory_test.Make (Cfg)
  module Ctrl = Sdram.Make (X)

  let create scope (i : Signal.t I.t) : Signal.t O.t =
    let read_w = Signal.wire 1 in
    let write_w = Signal.wire 1 in
    let addr_w = Signal.wire 21 in
    let din_w = Signal.wire 32 in
    let ctrl =
      Ctrl.hierarchical
        scope
        { Ctrl.I.clock = i.clock
        ; reset = i.reset
        ; read = read_w
        ; write = write_w
        ; addr = addr_w
        ; din = din_w
        ; byte_en = of_int ~width:4 0xF
        ; dq_in = i.dq_in
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
    let leds =
      X.normalize_led
        (concat_lsb
           [ test.heartbeat
           ; test.pass
           ; test.fail
           ; ctrl.ready
           ; ctrl.busy
           ; test.active
           ])
    in
    { O.leds
    ; dout = ctrl.dout
    ; done_ = ctrl.done_
    ; ready = ctrl.ready
    ; busy = ctrl.busy
    ; dq_out = ctrl.dq_out
    ; dq_oe = ctrl.dq_oe
    ; sdram_clk = ctrl.sdram_clk
    ; sdram_cke = ctrl.sdram_cke
    ; sdram_cs_n = ctrl.sdram_cs_n
    ; sdram_ras_n = ctrl.sdram_ras_n
    ; sdram_cas_n = ctrl.sdram_cas_n
    ; sdram_wen_n = ctrl.sdram_wen_n
    ; sdram_addr = ctrl.sdram_addr
    ; sdram_ba = ctrl.sdram_ba
    ; sdram_dqm = ctrl.sdram_dqm
    ; pass = test.pass
    ; fail = test.fail
    ; active = test.active
    }
  ;;
  let hierarchical scope (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"sdram_system" ~instance:"inst_sdram_system" create i
;;

end
