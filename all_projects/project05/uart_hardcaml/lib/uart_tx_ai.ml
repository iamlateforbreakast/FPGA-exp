open Hardcaml
open Hardcaml.Signal

module State = struct
  type t = IDLE | START | SEND_BYTE | STOP [@@deriving enumerate, compare]
end

let uart_tx ~(clk_fre_mhz : int) ~(baud_rate : int) (scope : Scope.t) (i : Signal.t Interface.t) =
  let clk = i.clk in
  let rst_n = i.rst_n in
  (* Hardcaml Reg_spec typically uses active high reset; 
     we invert rst_n here for the spec *)
  let spec = Reg_spec.create ~clock:clk ~reset:(~: rst_n) () in

  (* Constants *)
  let cycle_limit = (clk_fre_mhz * 1_000_000) / baud_rate in
  let cycle_limit_sig = const_int ~width:16 (cycle_limit - 1) in

  (* State and Registers *)
  let sm = Statemachine.create (module State) spec in
  let cycle_cnt = Always.Variable.reg spec ~width:16 in
  let bit_cnt = Always.Variable.reg spec ~width:3 in
  let tx_data_latch = Always.Variable.reg spec ~width:8 in
  let tx_data_ready = Always.Variable.reg spec ~width:1 in
  let tx_reg = Always.Variable.reg spec ~width:1 ~reset_value:High in

  (* Combinational logic for the bit selector *)
  let bit_to_send = mux bit_cnt.value (bits_msb_to_lsb tx_data_latch.value |> List.rev) in

  Always.(compile [
    (* Default: increment cycle counter *)
    cycle_cnt <-- cycle_cnt.value +: 1;

    sm.switch [
      IDLE, [
        tx_reg <-- High;
        if_ i.tx_data_valid [
          tx_data_latch <-- i.tx_data;
          tx_data_ready <-- Low;
          cycle_cnt <-- zero 16;
          sm.set_next START;
        ] [
          tx_data_ready <-- High;
        ];
      ];

      START, [
        tx_reg <-- Low;
        if_ (cycle_cnt.value ==: cycle_limit_sig) [
          cycle_cnt <-- zero 16;
          sm.set_next SEND_BYTE;
        ] [];
      ];

      SEND_BYTE, [
        tx_reg <-- bit_to_send;
        if_ (cycle_cnt.value ==: cycle_limit_sig) [
          cycle_cnt <-- zero 16;
          if_ (bit_cnt.value ==: const_int ~width:3 7) [
            bit_cnt <-- zero 3;
            sm.set_next STOP;
          ] [
            bit_cnt <-- bit_cnt.value +: 1;
          ];
        ] [];
      ];

      STOP, [
        tx_reg <-- High;
        if_ (cycle_cnt.value ==: cycle_limit_sig) [
          cycle_cnt <-- zero 16;
          tx_data_ready <-- High;
          sm.set_next IDLE;
        ] [];
      ];
    ];
  ]);

  (* Return the output signals *)
  { tx_data_ready = tx_data_ready.value; tx_pin = tx_reg.value }
