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

  module States = struct
    type t = S_IDLE | S_START | S_SEND_BYTE | S_STOP
    [@@deriving sexp_of, compare, enumerate]
  end

  let create
    ~build_mode
    scope
    { I.clock; resetn; tx_data; tx_data_valid }
    =
    let ( -- ) = Scope.naming scope in
    let open Always in
    let spec_with_clear = Reg_spec.create ~clear:(~:resetn) ~clock () in
    let state = reg spec_with_clear ~width:3 in
    let next_state = reg spec_with_clear ~width:3 in
    let cycle_counter = reg spec_with_clear ~width:16 in
    let bit_count = reg spec_with_clear ~width:3 in
    let tx_data_latch = reg spec_with_clear ~width:8 in
    let tx_reg = reg spec_with_clear ~width:1 in
    let next_state =
      mux state [
        (of_int ~width:3 S_IDLE,
          mux tx_data_valid
            (of_int ~width:3 S_START)
            (of_int ~width:3 S_IDLE)
        );
        (of_int ~width:3 S_START,
          mux (cycle_cnt ==:. (CYCLE - 1))
            (of_int ~width:3 S_SEND_BYTE)
            (of_int ~width:3 S_START)
        );
        (of_int ~width:3 S_SEND_BYTE,
          mux ((cycle_cnt ==:. (CYCLE - 1)) &: (bit_cnt ==:. 7))
            (of_int ~width:3 S_STOP)
            (of_int ~width:3 S_SEND_BYTE)
        );
        (of_int ~width:3 S_STOP,
          mux (cycle_cnt ==:. (CYCLE - 1))
            (of_int ~width:3 S_IDLE)
            (of_int ~width:3 S_STOP)
        )
      ] ~default:(of_int ~width:3 S_IDLE) in
    let tx_data_ready =
      reg_fb ~width:1 ~clock:clock ~reset:(~:resetn) (fun d ->
        mux2 (~:resetn)
        (zero 1)
        (
          mux2 (state ==:. S_IDLE)
            (mux2 tx_data_valid (zero 1) (ones 1))
            (
              mux2 ((state ==:. S_STOP) &: (cycle_cnt ==:. (CYCLE - 1)))
                (ones 1)
                d
            )
        )
      ) in
    (* Transmitter state machine *)
    { O.tx_pin = vdd; tx_data_ready = vdd }

  let hierarchical ?instance ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"uart_tx" ~scope (create ~build_mode)
end