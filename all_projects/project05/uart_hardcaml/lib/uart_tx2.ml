(* uart_tx.ml *)

open Hardcaml
open Hardcaml.Signal

module Make (X : Config.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; resetn : 'a
      ; tx_data : 'a [@bits 8]
      ; tx_data_valid : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { tx_pin : 'a
      ; tx_data_ready : 'a
      }
    [@@deriving hardcaml]
  end

  (* State definition *)
  module State = struct
    type t = IDLE | START | SEND_BYTE | STOP
    [@@deriving sexp_of, compare, enumerate]
  end

  let create _scope { I.clock; resetn; tx_data; tx_data_valid } =
    let spec = Reg_spec.create ~clock ~reset:(~:resetn) () in
    
    (* Parameters based on Config X *)
    let baud_rate = 115200 in
    let clk_freq_hz = 50 * 1_000_000 in
    let cycles_per_bit = clk_freq_hz / baud_rate in

    let sm = Always.State_machine.create (module State) spec in
    
    let cycle_cnt = Always.Variable.reg spec ~width:16 in
    let bit_cnt = Always.Variable.reg spec ~width:3 in
    let tx_data_latch = Always.Variable.reg spec ~width:8 in
    let tx_pin = Always.Variable.reg spec ~width:1 in
    let tx_data_ready = Always.Variable.wire ~default:gnd in

    let is_last_cycle = cycle_cnt.value ==:. (cycles_per_bit - 1) in
    let is_last_bit = bit_cnt.value ==:. 7 in

    Always.(compile [
      (* Default behavior *)
      cycle_cnt <-- cycle_cnt.value +:. 1;
      tx_pin <-- vdd;

      sm.switch [
        IDLE, [
          tx_data_ready <-- vdd;
          cycle_cnt <-- gnd;
          if_ tx_data_valid [
            tx_data_latch <-- tx_data;
            sm.set_next START;
          ] [];
        ];

        START, [
          tx_pin <-- gnd;
          if_ is_last_cycle [
            cycle_cnt <-- gnd;
            sm.set_next SEND_BYTE;
          ] [];
        ];

        SEND_BYTE, [
          (* Select bit from latch using mux *)
          tx_pin <-- mux bit_cnt.value (Array.to_list (bits_msb tx_data_latch.value |> List.rev));
          if_ is_last_cycle [
            cycle_cnt <-- 0;
            if_ is_last_bit [
              bit_cnt <-- 0;
              sm.set_next STOP;
            ] [
              bit_cnt <-- bit_cnt.value +:. 1;
            ];
          ] [];
        ];

        STOP, [
          tx_pin <-- vdd;
          if_ is_last_cycle [
            cycle_cnt <-- 0;
            sm.set_next IDLE;
          ] [];
        ];
      ]
    ]);

    { O.tx_pin = tx_pin.value; tx_data_ready = tx_data_ready.value }

  let hierarchical ?instance scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"uart_tx" ~scope (create scope) i
end
