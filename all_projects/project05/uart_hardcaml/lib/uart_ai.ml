open Hardcaml
open Hardcaml.Signal

module State = struct
  type t = IDLE | SEND | WAIT [@@deriving enumerate, compare]
end

let uart_test (clk : Signal.t) (rst : Signal.t) (uart_rx : Signal.t) =
  let spec = Reg_spec.create ~clock:clk ~reset:rst () in
  let clk_freq = 27_000_000 in
  
  (* String Data Preparation (Example 1) *)
  let message = "你好 Tang Nano 20K\r\n" in
  let data_num = String.length message in
  let send_data_rom = 
    Array.init data_num (fun i -> const_int ~width:8 (Char.to_int message.[i]))
    |> Array.to_list |> reverse
  in

  (* Registers and State *)
  let sm = Statemachine.create (module State) spec in
  let tx_cnt = Always.Variable.wire ~default:(zero 8) in
  let wait_cnt = Always.Variable.wire ~default:(zero 32) in
  let tx_data = Always.Variable.wire ~default:(zero 8) in
  let tx_data_valid = Always.Variable.wire ~default:(zero 1) in

  (* UART Instantiations (Internal logic assumed) *)
  (* Note: You would typically use a Hardcaml Circuit or Instance here *)
  let rx_data = wire 8 in
  let rx_data_valid = wire 1 in
  let tx_data_ready = wire 1 in

  (* Combinational logic for tx_str (ROM lookup) *)
  let tx_str = mux (tx_cnt.value) send_data_rom in

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

  (* Outputs *)
  {
    tx_data = reg spec tx_data.value;
    tx_data_valid = reg spec tx_data_valid.value;
    (* Connect to your UART TX module here *)
  }
