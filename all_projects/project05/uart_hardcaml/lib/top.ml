(* top.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { uart_tx : 'a
      }
    [@@deriving hardcaml]
  end
  
  module MyUart_tx = Uart_tx.Make(X)

  module State = struct
    type t = IDLE | SEND | WAIT [@@deriving sexp_of, enumerate, compare]
  end

  let message_rom ~index =
    let open Signal in
    let chars = X.message |> String.to_seq |> List.of_seq in
    let rom = List.map (fun c -> of_char c) chars in
    mux index rom

  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in

	  (* Constants *)
    let data_num = String.length X.message in

    (* Registers and State *)
    let sm = Always.State_machine.create (module State) sync_spec in
    let tx_cnt = Always.Variable.reg sync_spec ~enable:vdd ~width:8 in
    let wait_cnt = Always.Variable.reg sync_spec ~enable:vdd ~width:32 in
    let tx_data_valid = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in

    let tx_data = Always.Variable.wire ~default:(zero 8) in

    (* Instanciate UART TX *)
    let uart_tx = MyUart_tx.hierarchical scope (
      MyUart_tx.I.{ clock = input.clock
                  ; reset = input.reset
                  ; data = tx_data.value
                  ; data_valid = tx_data_valid.value
                  } 
    )
    in
    (* Combinational logic for tx_str (ROM lookup) *)
    let tx_str = message_rom ~index:tx_cnt.value in
    let tx_data_ready = uart_tx.data_ready in

    (* State Machine Logic *)
    Always.(compile [
      sm.switch [
        IDLE, [
          sm.set_next SEND;
        ];
        SEND, [
          wait_cnt <-- zero 32;
          tx_data <-- tx_str;
          if_ (tx_data_valid.value &: tx_data_ready) [
            if_ (tx_cnt.value <:. (data_num - 1)) [
              tx_cnt <-- tx_cnt.value +:. 1;
            ] [
              tx_cnt <-- zero 8;
              tx_data_valid <-- gnd;
              sm.set_next WAIT;
            ];
          ] [
            if_ (~: (tx_data_valid.value)) [
              tx_data_valid <-- vdd;
            ] [];
          ];
        ];
        WAIT, [
          wait_cnt <-- wait_cnt.value +:. 1;
          if_ (wait_cnt.value >=: (of_int ~width:32 X.clk_fre)) [
            sm.set_next SEND;
          ] [];
        ];
      ];
    ];
    );

    { O.uart_tx = uart_tx.pin }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
end
