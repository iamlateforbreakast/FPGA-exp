(* uart_tx.ml *)

open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

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

  (* State encoding *)
  module States = struct
    type t = S_IDLE | S_START | S_SEND_BYTE | S_STOP
    [@@deriving sexp_of, compare, enumerate]
  end

let create
    ~build_mode
    scope
    { I.clock; resetn; tx_data; tx_data_valid }
  =
    let width_state = 3 in
    let width_cycle = 16 in
    let width_bit = 3 in
    let width_data = 8 in

    (* Parameters *)
    let clk_fre = 50 in
    let baud_rate = 115200 in
    let cycle = clk_fre * 1_000_000 / baud_rate in

    (* Registers *)
    let state = reg ~width:width_state ~clock:clk ~reset:(~:rst_n) ~reset_to:(of_int ~width:width_state s_idle) in
    let next_state = wire width_state in
    let cycle_cnt = reg ~width:width_cycle ~clock:clk ~reset:(~:rst_n) ~reset_to:(zero width_cycle) in
    let bit_cnt = reg ~width:width_bit ~clock:clk ~reset:(~:rst_n) ~reset_to:(zero width_bit) in
    let tx_data_latch = reg ~width:width_data ~clock:clk ~reset:(~:rst_n) ~reset_to:(zero width_data) in
    let tx_reg = reg ~width:1 ~clock:clk ~reset:(~:rst_n) ~reset_to:(vdd) in

    (* Next state logic *)
    let next_state_val =
      mux state [
        (of_int ~width:width_state s_idle,
          mux tx_data_valid
            (of_int ~width:width_state s_start)
            (of_int ~width:width_state s_idle)
        );
        (of_int ~width:width_state s_start,
          mux (cycle_cnt ==:. (cycle - 1))
            (of_int ~width:width_state s_send_byte)
            (of_int ~width:width_state s_start)
        );
        (of_int ~width:width_state s_send_byte,
          mux ((cycle_cnt ==:. (cycle - 1)) &: (bit_cnt ==:. 7))
            (of_int ~width:width_state s_stop)
            (of_int ~width:width_state s_send_byte)
        );
        (of_int ~width:width_state s_stop,
          mux (cycle_cnt ==:. (cycle - 1))
            (of_int ~width:width_state s_idle)
            (of_int ~width:width_state s_stop)
        )
      ] ~default:(of_int ~width:width_state s_idle)
    in
    next_state <== next_state_val;

    (* State register update *)
    state <== next_state;

    (* tx_data_ready logic *)
    let tx_data_ready =
      reg_fb ~width:1 ~clock:clk ~reset:(~:rst_n) ~reset_to:(zero 1) (fun d ->
        mux2 (state ==:. s_idle)
          (mux2 tx_data_valid (zero 1) (ones 1))
          (
            mux2 ((state ==:. s_stop) &: (cycle_cnt ==:. (cycle - 1)))
              (ones 1)
              d
          )
      )
    in

    (* tx_data_latch logic *)
    tx_data_latch <== mux2 ((state ==:. s_idle) &: tx_data_valid) tx_data tx_data_latch;

    (* bit_cnt logic *)
    let bit_cnt_next =
      mux2 (state ==:. s_send_byte)
        (mux2 (cycle_cnt ==:. (cycle - 1)) (bit_cnt +:. 1) bit_cnt)
        (zero width_bit)
    in
    bit_cnt <== bit_cnt_next;

    (* cycle_cnt logic *)
    let cycle_cnt_next =
      mux2 ((state ==:. s_send_byte) &: (cycle_cnt ==:. (cycle - 1)) |: (next_state <>: state))
        (zero width_cycle)
        (cycle_cnt +:. 1)
    in
    cycle_cnt <== cycle_cnt_next;

    (* tx_reg logic *)
    let tx_reg_next =
      mux state [
        (of_int ~width:width_state s_idle, vdd);
        (of_int ~width:width_state s_stop, vdd);
        (of_int ~width:width_state s_start, gnd);
        (of_int ~width:width_state s_send_byte, select tx_data_latch bit_cnt);
      ] ~default:vdd
    in
    tx_reg <== tx_reg_next;

    (* Outputs *)
    let tx_pin = tx_reg in

    (* Return outputs as a record *)
    { tx_data_ready; tx_pin }

  let hierarchical ?instance ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"uart_tx" ~scope (create ~build_mode)
end
