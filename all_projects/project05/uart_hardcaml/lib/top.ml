(* top.ml *)
open Hardcaml
open Signal
open Uart_tx

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
      { tx_pin : 'a
      ; tx_ready : 'a
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
    let _wait_time  = if (X.is_simulation = false) then 1
	                   else (X.cycle_period / X.clk_fre) - 1 in
    let data_num = String.length X.message in

    (* Registers and State *)
    let sm = Always.State_machine.create (module State) sync_spec in
    let tx_cnt = Always.Variable.wire ~default:(zero 8) in
    let wait_cnt = Always.Variable.wire ~default:(zero 32) in
    let tx_data = Always.Variable.wire ~default:(zero 8) in
    let tx_data_valid = Always.Variable.wire ~default:(zero 1) in

    (* Instanciate UART TX *)
    let uart_tx = MyUart_tx.hierarchical scope (
      MyUart_tx.I.{ baud_rate = const_int ~width:32 X.uart_baudrate
        ; clk_freq = const_int ~width:32 X.clk_freq } 
    )
    in
    (* Combinational logic for tx_str (ROM lookup) *)
    let tx_str = message_rom ~index:tx_cnt.value in

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
              tx_cnt <-- tx_cnt.value +: 1;
            ] [
              tx_cnt <-- zero 8;
              tx_data_valid <-- Low;
              sm.set_next WAIT;
            ];
          ] [
            if_ (~: (tx_data_valid.value)) [
              tx_data_valid <-- High;
            ] [];
          ];
        ];
        WAIT, [
          wait_cnt <-- wait_cnt.value +: 1;
          if_ (rx_data_valid) [
            tx_data_valid <-- High;
            tx_data <-- rx_data;
          ] [
            if_ (tx_data_valid.value &: tx_data_ready) [
              tx_data_valid <-- Low;
            ] [
              if_ (wait_cnt.value >=: const_int ~width:32 clk_freq) [
                sm.set_next SEND;
              ] [];
            ];
          ];
        ];
      ];
    ]);
    { O.tx_pin = uart_tx.Uart_tx.data; tx_ready = uart_tx.Uart_tx.data_ready }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
end
