(** Spw_link_fsm — SpaceWire link-layer finite state machine.

    Implements the six-state machine defined in ECSS-E-ST-50-12C §8.5:

      ErrorReset → ErrorWait → Ready → Started → Connecting → Run

    Transitions are driven by:
      • gotNULL   — a valid NULL (ESC+FCT) character pair was received
      • gotFCT    — a Flow-Control Token was received
      • gotData   — a data character was received
      • gotTimecode — a time-code was received
      • credit    — credit counter state
      • timeout   — a 6.4 µs timer (in ErrorWait)
      • errors    — disconnect, parity, credit

    The FSM also controls which characters the TX path is allowed to send.

    Ports
    ─────
    All inputs/outputs are in the system clock domain.  The TX/RX character
    streams cross into this domain via the FIFOs. *)

open Hardcaml
open Signal
open Hardcaml.Always
open Spw_constants

(* ── SpaceWire control character identifiers ──────────────────────────── *)
(* flag=1 characters use bits [3:0] of the control nibble *)
module Ctrl_char = struct
  let fct  = 0b0001  (* Flow Control Token  *)
  let eop  = 0b0010  (* End Of Packet       *)
  let eep  = 0b0100  (* Error End of Packet *)
  let esc  = 0b0000  (* Escape (for NULL)   *)
  let null_seq = [esc; fct]  (* NULL = ESC followed immediately by FCT *)
end

(* ── 6.4 µs timeout counter ──────────────────────────────────────────── *)
(* At 54 MHz system clock, 6.4 µs = ~346 cycles.  Parameterise. *)
let error_wait_cycles = 346

module I = struct
  type 'a t =
    { clock        : 'a [@bits 1]
    ; clear        : 'a [@bits 1]
    (* Configuration *)
    ; link_en      : 'a [@bits 1]  (** from CTRL register bit 0 *)
    ; auto_start   : 'a [@bits 1]  (** from LINK_CFG bit 6      *)
    (* RX character path (from char_rx, in sys-clock domain via async FIFO) *)
    ; rx_char_valid   : 'a [@bits 1]
    ; rx_char_data    : 'a [@bits 9]  (** [8]=flag, [7:0]=payload *)
    ; rx_parity_err   : 'a [@bits 1]
    (* TX path handshake *)
    ; tx_char_ready   : 'a [@bits 1]  (** char_tx is idle and ready *)
    (* Credit tracking *)
    ; tx_credit_init  : 'a [@bits 3]  (** initial TX credit (normally 7) *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { link_state      : 'a [@bits 3]  (** current FSM state, see Link_state *)
    (* TX character requests to char_tx module *)
    ; tx_send_null    : 'a [@bits 1]  (** request to send a NULL            *)
    ; tx_send_fct     : 'a [@bits 1]  (** request to send an FCT            *)
    ; tx_send_data_ok : 'a [@bits 1]  (** data characters may be sent       *)
    (* Error outputs *)
    ; disconnect_err  : 'a [@bits 1]
    ; credit_err      : 'a [@bits 1]
    ; parity_err_out  : 'a [@bits 1]
    (* Credit *)
    ; rx_credit_grant : 'a [@bits 1]  (** pulse: grant one credit to peer   *)
    }
  [@@deriving hardcaml]
end

let create (i : _ I.t) : _ O.t =
  let spec = reg_spec ~clock:i.clock ~clear:i.clear () in

  (* ── State register ────────────────────────────────────────────────── *)
  let state     = Always.Variable.reg ~width:Link_state.bits spec ~enable:vdd in
  let is_state s = state.value ==: of_int ~width:Link_state.bits s in

  (* ── Received-character helpers ────────────────────────────────────── *)
  let rx_flag   = bit i.rx_char_data 8           -- "rx_flag"   in
  let rx_ctrl_n = sel_bottom i.rx_char_data 4    -- "rx_ctrl_n" in  (* lower 4 bits *)
  let rx_is_fct = rx_flag &: (rx_ctrl_n ==: of_int ~width:4 Ctrl_char.fct) in
  let rx_is_esc = rx_flag &: (rx_ctrl_n ==: of_int ~width:4 Ctrl_char.esc) in
  let rx_is_data = (~: rx_flag) &: i.rx_char_valid -- "rx_is_data" in

  (* NULL detection: ESC followed immediately by FCT *)
  let last_was_esc = reg spec ~enable:vdd
    (mux2 i.rx_char_valid
      (mux2 rx_is_esc vdd gnd)
      (reg spec ~enable:vdd gnd))
    -- "last_was_esc"
  in
  let got_null = i.rx_char_valid &: rx_is_fct &: last_was_esc -- "got_null" in
  let got_fct  = i.rx_char_valid &: rx_is_fct &: (~: last_was_esc) -- "got_fct" in

  (* ── Error signals ─────────────────────────────────────────────────── *)
  let parity_err   = i.rx_parity_err  -- "parity_err"   in
  (* Disconnect: no transition seen for > 850 ns.  We approximate with a
     simple counter reset by the DS-decoder transition pulse.  For now
     we wire disconnect_err = 0; tie in the DS decoder's transition output
     at the top level. *)
  let disconnect_err = gnd -- "disconnect_err_stub" in
  let any_error = parity_err |: disconnect_err      -- "any_error"   in

  (* ── 6.4 µs ErrorWait timer ────────────────────────────────────────── *)
  let timer_w      = num_bits_to_represent error_wait_cycles in
  let timer        = Always.Variable.reg ~width:timer_w spec ~enable:vdd in
  let timer_done   = timer.value ==: of_int ~width:timer_w 0  -- "timer_done" in

  (* ── Credit counter (TX credits available) ─────────────────────────── *)
  let credit_w = 4 in  (* max 7 outstanding FCTs, 3 bits + 1 headroom *)
  let tx_credit = Always.Variable.reg ~width:credit_w spec ~enable:vdd in
  let credit_err_sig = (tx_credit.value ==: zero credit_w) &: got_fct
                       -- "credit_err_sig" in

  (* ── RX credit: how many FCTs we've sent to the peer ──────────────── *)
  let rx_credit = Always.Variable.reg ~width:credit_w spec ~enable:vdd in
  let rx_credit_grant = Always.Variable.wire ~default:gnd in

  (* ── TX request wires ──────────────────────────────────────────────── *)
  let tx_send_null    = Always.Variable.wire ~default:gnd in
  let tx_send_fct     = Always.Variable.wire ~default:gnd in
  let tx_send_data_ok = Always.Variable.wire ~default:gnd in

  (* ─────────────────────────── FSM ─────────────────────────────────── *)
  compile [
    (* Default outputs *)
    tx_send_null    <-- gnd;
    tx_send_fct     <-- gnd;
    tx_send_data_ok <-- gnd;
    rx_credit_grant <-- gnd;

    (* Timer counts down *)
    when_ (~: timer_done) [
      timer <-- timer.value -: one timer_w
    ];

    switch state.value [
      (* ── ErrorReset ── *)
      of_int ~width:Link_state.bits Link_state.error_reset, [
        tx_send_null <-- vdd;
        timer        <-- of_int ~width:timer_w error_wait_cycles;
        tx_credit    <-- uresize i.tx_credit_init credit_w;
        rx_credit    <-- zero credit_w;
        when_ i.link_en [
          state <-- of_int ~width:Link_state.bits Link_state.error_wait
        ]
      ];
      (* ── ErrorWait ── *)
      of_int ~width:Link_state.bits Link_state.error_wait, [
        tx_send_null <-- vdd;
        when_ any_error [
          state <-- of_int ~width:Link_state.bits Link_state.error_reset
        ];
        when_ timer_done [
          state <-- of_int ~width:Link_state.bits Link_state.ready
        ]
      ];
      (* ── Ready ── *)
      of_int ~width:Link_state.bits Link_state.ready, [
        tx_send_null <-- vdd;
        when_ any_error [
          state <-- of_int ~width:Link_state.bits Link_state.error_reset
        ];
        when_ got_null [
          state <-- of_int ~width:Link_state.bits Link_state.started
        ]
      ];
      (* ── Started ── *)
      of_int ~width:Link_state.bits Link_state.started, [
        tx_send_null <-- vdd;
        tx_send_fct  <-- vdd;
        when_ any_error [
          state <-- of_int ~width:Link_state.bits Link_state.error_reset
        ];
        when_ (got_null &: got_fct) [
          state <-- of_int ~width:Link_state.bits Link_state.connecting
        ]
      ];
      (* ── Connecting ── *)
      of_int ~width:Link_state.bits Link_state.connecting, [
        tx_send_null <-- vdd;
        tx_send_fct  <-- vdd;
        when_ any_error [
          state <-- of_int ~width:Link_state.bits Link_state.error_reset
        ];
        when_ got_fct [
          when_ credit_err_sig [
            state <-- of_int ~width:Link_state.bits Link_state.error_reset
          ];
          when_ (~: credit_err_sig) [
            tx_credit <-- tx_credit.value +: one credit_w;
            state     <-- of_int ~width:Link_state.bits Link_state.run
          ]
        ]
      ];
      (* ── Run ── *)
      of_int ~width:Link_state.bits Link_state.run, [
        tx_send_data_ok <-- vdd;
        (* Receive FCT: increment TX credit *)
        when_ got_fct [
          when_ credit_err_sig [
            state <-- of_int ~width:Link_state.bits Link_state.error_reset
          ];
          when_ (~: credit_err_sig) [
            tx_credit <-- tx_credit.value +: one credit_w
          ]
        ];
        (* Receive data: decrement TX credit, grant RX credit back *)
        when_ rx_is_data [
          rx_credit_grant <-- vdd;
          tx_send_fct     <-- vdd
        ];
        when_ any_error [
          state <-- of_int ~width:Link_state.bits Link_state.error_reset
        ]
      ]
    ]
  ];

  { O.
    link_state      = state.value
  ; tx_send_null    = tx_send_null.value
  ; tx_send_fct     = tx_send_fct.value
  ; tx_send_data_ok = tx_send_data_ok.value
  ; disconnect_err
  ; credit_err      = credit_err_sig
  ; parity_err_out  = parity_err
  ; rx_credit_grant = rx_credit_grant.value
  }

let circuit () =
  let module C = Circuit.With_interface (I) (O) in
  C.create_exn ~name:"spw_link_fsm" create
