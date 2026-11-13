open Hardcaml
open Signal
open Base

(** Non-bursting, auto-precharge SDRAM controller for the 64 Mbit (8 MiB,
    32-bit) SDRAM embedded in the Tang Nano 20K's GW2AR-18.

    The controller runs from a fabric clock at 4x the SDRAM clock.  A two-bit
    [phase] counter divides it down: the SDRAM clock is high in phases 1-2 and
    low in phases 3-0.  All commands and data are launched at phase 0, giving
    one fabric cycle of setup before the CK rising edge at phase 1, and read
    data is sampled at phase 2 (the centre of the CK high phase).  No
    ODDR/IDDR primitives are needed, which also makes the controller
    simulatable with an ordinary behavioural memory model.

    Timing is conservative for the 20.25 MHz SDRAM clock produced by an
    81 MHz PLL: tRCD = tRP = tRFC = tMRD = tWR = 2 CK cycles, CL = 2. *)
module Make (X : Config_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; read : 'a
      ; write : 'a
      ; addr : 'a [@bits 21]  (** 32-bit word address *)
      ; din : 'a [@bits 32]
      ; byte_en : 'a [@bits 4]
      ; dq_in : 'a [@bits 32]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { dout : 'a [@bits 32]
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
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | INIT_DELAY
      | INIT_PRECHARGE
      | INIT_WAIT_RP
      | INIT_REFRESH1
      | INIT_WAIT_RFC1
      | INIT_REFRESH2
      | INIT_WAIT_RFC2
      | INIT_MRS
      | INIT_WAIT_MRD
      | IDLE
      | ACTIVATE
      | ACTIVATE_WAIT
      | DO_READ
      | READ_WAIT
      | DO_WRITE
      | WRITE_WAIT
      | WAIT_IDLE
      | REFRESH
      | REFRESH_WAIT
    [@@deriving sexp_of, compare, enumerate]
  end

  (* 200 us power-on delay, in fabric clock cycles. *)
  let init_cycles = X.pll_freq / 5000

  (* Auto-refresh at least once every 15 us.  [refresh_cycles] is in SDRAM
     (CK) cycles, which is what the [ce]-enabled counter below counts. *)
  let refresh_cycles = X.pll_freq / 4 * 15 / 1_000_000
  let t_rcd = 2
  let t_rp = 2
  let t_rfc = 2
  let t_mrd = 2
  let t_wr = 2
  let cl = 2
  let mode_reg = 0x020 (* BL=1, sequential, CL=2 *)

  let create _scope (i : Signal.t I.t) : Signal.t O.t =
    let open Always in
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in
    let sm = State_machine.create (module States) spec ~enable:vdd in
    let phase = Variable.reg spec ~width:2 in
    let wait = Variable.reg spec ~width:4 in
    let read_cnt = Variable.reg spec ~width:4 in
    let capture = Variable.reg spec ~width:1 in
    let init_cnt = Variable.reg spec ~width:15 in
    let init_done = Variable.reg spec ~width:1 in
    let refresh_cnt = Variable.reg spec ~width:10 in
    let is_write = Variable.reg spec ~width:1 in
    let addr_r = Variable.reg spec ~width:21 in
    let din_r = Variable.reg spec ~width:32 in
    let be_r = Variable.reg spec ~width:4 in
    let dout_r = Variable.reg spec ~width:32 in
    let done_r = Variable.reg spec ~width:1 in
    let dq_oe_r = Variable.reg spec ~width:1 in
    let ce = phase.value ==:. 0 in
    let is = sm.is in
    let refresh_pending = refresh_cnt.value >=: of_int ~width:10 refresh_cycles in
    let ready = is IDLE &: init_done.value &: ~: refresh_pending in
    let cmd_ras =
      ~:
        ( is INIT_PRECHARGE
        |: is INIT_REFRESH1
        |: is INIT_REFRESH2
        |: is INIT_MRS
        |: is ACTIVATE
        |: is REFRESH )
    in
    let cmd_cas =
      ~:
        ( is INIT_REFRESH1
        |: is INIT_REFRESH2
        |: is INIT_MRS
        |: is DO_READ
        |: is DO_WRITE
        |: is REFRESH )
    in
    let cmd_wen = ~: (is INIT_PRECHARGE |: is INIT_MRS |: is DO_WRITE) in
    let col = select addr_r.value 7 0 in
    let auto_precharge_addr = concat_msb [ vdd; gnd; col; gnd ] in
    let sdram_addr =
      mux2
        (is INIT_PRECHARGE)
        (of_int ~width:11 0x400)
        (mux2
           (is INIT_MRS)
           (of_int ~width:11 mode_reg)
           (mux2
              (is ACTIVATE)
              (select addr_r.value 18 8)
              (mux2
                 (is DO_READ |: is DO_WRITE)
                 auto_precharge_addr
                 (of_int ~width:11 0))))
    in
    let sdram_ba =
      mux2
        (is ACTIVATE |: is DO_READ |: is DO_WRITE)
        (select addr_r.value 20 19)
        (of_int ~width:2 0)
    in
    let sdram_dqm = mux2 (is DO_WRITE) (~:(be_r.value)) (of_int ~width:4 0) in
    let sdram_clk = (phase.value ==:. 1) |: (phase.value ==:. 2) in
    let () =
      compile
        [ phase <-- (phase.value +:. 1)
        ; (* Saturating 200 us counter.  Runs on every fabric cycle. *)
          if_ (init_cnt.value <: of_int ~width:15 init_cycles)
            [ init_cnt <-- (init_cnt.value +:. 1) ]
            []
        ; (* Read data is valid at the centre of the CK high phase.  Capture
             it at phase 2; the state machine picks [capture] up at the next
             phase-0 edge. *)
          when_
            ((phase.value ==:. 2)
            &: (is READ_WAIT)
            &: (read_cnt.value ==: of_int ~width:4 cl))
            [ dout_r <-- i.dq_in; capture <--. 1 ]
        ; when_ ce
            [ (* Defaults.  State actions below override these where needed. *)
              done_r <--. 0;
              dq_oe_r <--. 0;
              capture <--. 0;
              read_cnt <--. 0;
              if_ (wait.value <>:. 0) [ wait <-- (wait.value -:. 1) ] [];
              if_ (refresh_cnt.value <: of_int ~width:10 refresh_cycles)
                [ refresh_cnt <-- (refresh_cnt.value +:. 1) ]
                [];
              sm.switch
                [ ( INIT_DELAY
                  , [ if_
                        (init_cnt.value >=: of_int ~width:15 init_cycles)
                        [ wait <--. t_rp
                        ; sm.set_next INIT_PRECHARGE
                        ]
                        []
                    ] )
                ; ( INIT_PRECHARGE
                  , [ wait <--. t_rp
                    ; sm.set_next INIT_WAIT_RP
                    ] )
                ; ( INIT_WAIT_RP
                  , [ if_ (wait.value ==:. 0) [ sm.set_next INIT_REFRESH1 ] [] ]
                  )
                ; ( INIT_REFRESH1
                  , [ wait <--. t_rfc
                    ; sm.set_next INIT_WAIT_RFC1
                    ] )
                ; ( INIT_WAIT_RFC1
                  , [ if_ (wait.value ==:. 0) [ sm.set_next INIT_REFRESH2 ] [] ]
                  )
                ; ( INIT_REFRESH2
                  , [ wait <--. t_rfc
                    ; sm.set_next INIT_WAIT_RFC2
                    ] )
                ; ( INIT_WAIT_RFC2
                  , [ if_ (wait.value ==:. 0) [ sm.set_next INIT_MRS ] [] ] )
                ; ( INIT_MRS
                  , [ wait <--. t_mrd
                    ; sm.set_next INIT_WAIT_MRD
                    ] )
                ; ( INIT_WAIT_MRD
                  , [ if_ (wait.value ==:. 0)
                        [ init_done <--. 1
                        ; refresh_cnt <--. 0
                        ; sm.set_next IDLE
                        ]
                        []
                    ] )
                ; ( IDLE
                  , [ if_ (i.read |: i.write)
                        [ addr_r <-- i.addr
                        ; din_r <-- i.din
                        ; be_r <-- i.byte_en
                        ; is_write <-- i.write
                        ; wait <--. t_rcd
                        ; sm.set_next ACTIVATE
                        ]
                        [ if_ refresh_pending
                            [ wait <--. t_rfc
                            ; sm.set_next REFRESH
                            ]
                            []
                        ]
                    ] )
                ; ( ACTIVATE
                  , [ sm.set_next ACTIVATE_WAIT ] )
                ; ( ACTIVATE_WAIT
                  , [ if_ (wait.value ==:. 0)
                        [ if_ is_write.value
                            [ sm.set_next DO_WRITE ]
                            [ sm.set_next DO_READ ]
                        ]
                        []
                    ] )
                ; ( DO_READ
                  , [ read_cnt <--. 1
                    ; sm.set_next READ_WAIT
                    ] )
                ; ( READ_WAIT
                  , [ read_cnt <-- (read_cnt.value +:. 1)
                    ; if_ capture.value
                        [ wait <--. t_rp
                        ; dq_oe_r <--. 0
                        ; sm.set_next WAIT_IDLE
                        ]
                        []
                    ] )
                ; ( DO_WRITE
                  , [ dq_oe_r <--. 1
                    ; wait <--. (t_wr + t_rp)
                    ; sm.set_next WRITE_WAIT
                    ] )
                ; ( WRITE_WAIT
                  , [ if_ (wait.value ==:. 0)
                        [ dq_oe_r <--. 0
                        ; done_r <--. 1
                        ; sm.set_next IDLE
                        ]
                        []
                    ] )
                ; ( WAIT_IDLE
                  , [ if_ (wait.value ==:. 0)
                        [ done_r <--. 1; sm.set_next IDLE ]
                        []
                    ] )
                ; ( REFRESH
                  , [ wait <--. t_rfc
                    ; sm.set_next REFRESH_WAIT
                    ] )
                ; ( REFRESH_WAIT
                  , [ if_ (wait.value ==:. 0)
                        [ refresh_cnt <--. 0; sm.set_next IDLE ]
                        []
                    ] )
                ]
            ]
        ]
    in
    { O.dout = dout_r.value
    ; done_ = done_r.value
    ; ready
    ; busy = ~: ready
    ; dq_out = din_r.value
    ; dq_oe = dq_oe_r.value |: is DO_WRITE
    ; sdram_clk
    ; sdram_cke = vdd
    ; sdram_cs_n = gnd
    ; sdram_ras_n = cmd_ras
    ; sdram_cas_n = cmd_cas
    ; sdram_wen_n = cmd_wen
    ; sdram_addr
    ; sdram_ba
    ; sdram_dqm
    }
  ;;
  let hierarchical scope (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"sdram" ~instance:"inst_sdram" create i
;;

end
