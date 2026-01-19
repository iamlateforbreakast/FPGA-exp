open Base
open Hardcaml
open Signal

(* Define Constants *)
module Constants = struct
  let rascas_delay = 1
  let cas_latency = 2
  let mode = Const { constant = "01000100000" } (* NO_WRITE_BURST, OP, CAS, SEQ, BL1 *)

  let state_idle = 0
  let state_cmd_cont = state_idle + rascas_delay
  let state_read = state_cmd_cont + cas_latency + 1
  let state_last = state_read

  let cmd_inhibit = Const { constant = "1111" }
  let cmd_nop = Const { constant = "0111" }
  let cmd_active = Const { constant = "0011" }
  let cmd_read = Const { constant = "0101" }
  let cmd_write = Const { constant = "0100" }
  let cmd_precharge = Const { constant = "0010" }
  let cmd_auto_refresh = Const { constant = "0001" }
  let cmd_load_mode = Const { constant = "0000" }
end

(* Module Interfaces *)
module I = struct
  type 'a t = {
    clk : 'a;
    reset_n : 'a;
    phase : 'a; [@bits 3]
    sd_data_in : 'a; [@bits 32]
    din : 'a; [@bits 16]
    addr : 'a; [@bits 22]
    ds : 'a; [@bits 2]
    oe : 'a;
    we : 'a;
  } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    sd_clk : 'a;
    sd_cke : 'a;
    sd_data_out : 'a; [@bits 32]
    sd_data_oe : 'a;
    sd_addr : 'a; [@bits 11]
    sd_dqm : 'a; [@bits 4]
    sd_ba : 'a; [@bits 2]
    sd_cs : 'a;
    sd_we : 'a;
    sd_ras : 'a;
    sd_cas : 'a;
    ready : 'a;
    dout : 'a; [@bits 16]
  } [@@deriving sexp_of, hardcaml]
end

let create (i : Signal.t I.t) =
  let open Constants in
  let spec = Reg_spec.create ~clock:i.clk ~clearance:Clearance.None () in
  
  (* State registers *)
  let state = Always.Variable.reg spec ~width:3 () in
  let init_state = Always.Variable.reg spec ~width:5 () in
  let sd_cmd = Always.Variable.reg spec ~width:4 () in
  let sd_addr = Always.Variable.reg spec ~width:11 () in
  let sd_ba = Always.Variable.reg spec ~width:2 () in
  let dout_d = Always.Variable.reg spec ~width:16 () in

  (* Logic for sd_data bus Selection *)
  let sd_data_mux_in = mux i.addr.:(0,0) [ i.sd_data_in.:(31,16); i.sd_data_in.:(15,0) ] in

  (* Combinational Logic *)
  let ready = i.init_state ==: consti 5 0 in
  
  Always.(compile [
    sd_cmd <-- cmd_inhibit;

    if_ (i.reset_n ==: (bit gnd)) [
      init_state <-- consti 5 0x1f;
      state      <-- consti 3 state_idle;
    ] [
      (* Increment state if initializing *)
      if_ (init_state.value <>: consti 5 0) [
        state <-- state.value +: 1;
      ] [];

      if_ ((state.value ==: consti 3 state_last) && (init_state.value <>: consti 5 0)) [
        init_state <-- init_state.value -: 1;
      ] [];

      (* Init Sequence Logic *)
      if_ (init_state.value <>: consti 5 0) [
        if_ (state.value ==: consti 3 state_idle) [
          if_ (init_state.value ==: consti 5 13) [
            sd_cmd <-- cmd_precharge;
            sd_addr <-- (consti 1 1) @: (zero 10);
          ] [];
          if_ (init_state.value ==: consti 5 2) [
            sd_cmd <-- cmd_load_mode;
            sd_addr <-- mode;
          ] [];
        ] [];
      ] [
        (* Normal Operation *)
        if_ (state.value ==: consti 3 state_idle) [
          if_ (i.phase ==: consti 3 2) [
            if_ (i.oe |: i.we) [
              sd_cmd <-- cmd_active;
              sd_addr <-- i.addr.:(19,9);
              sd_ba   <-- i.addr.:(21,20);
              state   <-- consti 3 1;
            ] [
              sd_cmd <-- cmd_auto_refresh;
            ]
          ] []
        ] [
          state <-- state.value +: 1;

          if_ (state.value ==: consti 3 state_cmd_cont) [
            sd_cmd  <-- mux i.we [cmd_read; cmd_write];
            sd_addr <-- (constb "100") @: i.addr.:(8,1);
          ] [];

          if_ (state.value >: consti 3 state_cmd_cont &&: state.value <: consti 3 state_read) [
            sd_cmd <-- cmd_nop;
          ] [];

          if_ (state.value ==: consti 3 state_last) [
            state <-- consti 3 state_idle;
          ] [];

          if_ (state.value ==: consti 3 state_read &&: ~: (i.we)) [
            dout_d <-- sd_data_mux_in;
          ] [];
        ]
      ]
    ]
  ]);

  (* Output Assignments *)
  let dout = mux (state.value ==: consti 3 state_read &&: ~:(i.we)) 
    [ dout_d.value; sd_data_mux_in ] in

  let dqm = mux i.we 
    [ constb "0000"; 
      mux i.addr.:(0,0) [ (i.ds @: constb "11"); (constb "11" @: i.ds) ] 
    ] in

  { O.
    sd_clk = ~: (i.clk);
    sd_cke = vdd;
    sd_data_out = i.din @: i.din;
    sd_data_oe = i.we;
    sd_addr = sd_addr.value;
    sd_dqm = dqm;
    sd_ba = sd_ba.value;
    sd_cs  = (sd_cmd.value).:(3,3);
    sd_ras = (sd_cmd.value).:(2,2);
    sd_cas = (sd_cmd.value).:(1,1);
    sd_we  = (sd_cmd.value).:(0,0);
    ready;
    dout;
  }
