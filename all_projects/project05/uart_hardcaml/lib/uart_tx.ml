(* uart_tx.ml *)

open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; data : 'a [@bits 8]
      ; data_valid : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { pin : 'a
      ; data_ready : 'a
      }
    [@@deriving hardcaml]
  end

  (* State encoding *)
  module States = struct
    type t = S_IDLE | S_START | S_SEND_BYTE | S_STOP
    [@@deriving sexp_of, compare, enumerate]
  end

  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in
    
    (* Constants: Tang Nano 20K @ 27MHz, 115200 Baud *)
    let clks_per_bit = X.uart_fre / X.baud_rate in
    
    (* FSM and Variables *)
    let sm = Always.State_machine.create (module States) sync_spec in
    let bit_cnt = Always.Variable.reg sync_spec ~enable:vdd ~width:3 in
    let clk_cnt = Always.Variable.reg sync_spec ~enable:vdd ~width:16 in
    let tx_reg  = Always.Variable.reg sync_spec ~enable:vdd ~width:8 in
    let tx_pin  = Always.Variable.wire ~default:vdd in
    (* let ready   = Always.Variable.wire ~default:gnd in *)
    let ready = sm.is S_IDLE in
    
    (* Debug *)
    let _ = Signal.(bit_cnt.value -- "bit_cnt") in
    let _ = Signal.(clk_cnt.value -- "clk_cnt") in
    let _ = Signal.(tx_reg.value -- "tx_reg") in
    let _ = Signal.(ready.value -- "ready") in
    let _ = Signal.(sm.current -- "state") in
    
    Always.(compile [
      sm.switch [
        S_IDLE, [
          ready <-- vdd;
          clk_cnt <-- zero 16;
          bit_cnt <-- zero 3;
          if_ input.data_valid [
            tx_reg <-- input.data;
            sm.set_next S_START;
          ] [];
        ];
        S_START, [
          tx_pin <-- gnd; (* Start bit is Low *)
          if_ (clk_cnt.value ==: (of_int ~width:16 (clks_per_bit - 1))) [
            clk_cnt <-- zero 16;
            sm.set_next S_SEND_BYTE;
          ] [
            clk_cnt <-- clk_cnt.value +:. 1;
          ];
        ];
        S_SEND_BYTE, [
          (* Shift out data bits LSB first *)
          tx_pin <-- mux (bit_cnt.value) (bits_lsb tx_reg.value);
          if_ (clk_cnt.value ==: (of_int ~width:16 (clks_per_bit - 1))) [
            clk_cnt <-- zero 16;
            if_ (bit_cnt.value ==: (of_int ~width:3 7)) [
              sm.set_next S_STOP;
            ] [
              bit_cnt <-- bit_cnt.value +:. 1;
            ];
          ] [
            clk_cnt <-- clk_cnt.value +:. 1;
          ];
        ];
        S_STOP, [
          tx_pin <-- vdd; (* Stop bit is High *)
          if_ (clk_cnt.value ==: (of_int ~width:16 (clks_per_bit - 1))) [
            sm.set_next S_IDLE;
          ] [
            clk_cnt <-- clk_cnt.value +:. 1;
          ];
        ];
      ];
    ];);

    { O.pin = tx_pin.value; data_ready = ready.value }
  
  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"uart_tx" ~instance:"inst1" create i
end
