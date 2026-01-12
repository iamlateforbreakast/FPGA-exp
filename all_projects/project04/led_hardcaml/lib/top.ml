(* svo_hdmi.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clk : 'a
      ; resetn : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { data : 'a
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (input : _ I.t) =
    let spec = Reg_spec.create ~clock:input.clk ~reset:input.reset () in

    (* Constants calculations *)
    let f = float_of_int config.clk_fre /. 1_000_000. in
    let delay_1_high = Int.of_float (f *. 0.85) - 1 in
    let delay_1_low  = Int.of_float (f *. 0.40) - 1 in
    let delay_0_high = Int.of_float (f *. 0.40) - 1 in
    let delay_0_low  = Int.of_float (f *. 0.85) - 1 in
    let delay_reset  = (config.clk_fre / 10) - 1 in

    (* State Machine Definition *)
    module State = struct
      type t = IDLE | DATA_SEND | BIT_SEND_HIGH | BIT_SEND_LOW
      [@@deriving enumerate, compare]
    end
    let sm = Statemachine.create (module State) spec in

    (* Registers *)
    let bit_send    = Always.Variable.reg spec ~width:9 in
    let data_send   = Always.Variable.reg spec ~width:9 in
    let clk_count   = Always.Variable.reg spec ~width:32 in
    let data_reg    = Always.Variable.reg spec ~width:1 in
    let ws_data     = Always.Variable.reg spec ~width:24 in
    let ws_valid    = Always.Variable.reg spec ~width:1 in

    (* Helper to index the data bit *)
    let current_bit_is_high = mux bit_send.value (bits_msb ws_data.value) in

    (* Logic Description *)
    Always.(compile [
      sm.switch [
        IDLE, [
          data_reg <-- gnd;
          if_ (clk_count.value <: (consti 32 delay_reset)) [
            clk_count <-- clk_count.value +: 1;
          ] [
            clk_count <-- (consti 32 0);
            if_ (ws_data.value <>: input.color |: ~:(ws_valid.value)) [
              ws_valid <-- vdd;
              ws_data  <-- input.color;
              sm.set_next DATA_SEND;
            ] [];
          ];
        ];

        DATA_SEND, [
          if_ (data_send.value >: (consti 9 config.ws2812_num) &: 
               bit_send.value ==: (consti 9 config.ws2812_width)) [
            clk_count <-- (consti 32 0);
            data_send <-- (consti 9 0);
            bit_send  <-- (consti 9 0);
            sm.set_next IDLE;
          ] @@ elif (bit_send.value <: (consti 9 config.ws2812_width)) [
            sm.set_next BIT_SEND_HIGH;
          ] [
            data_send <-- data_send.value +: 1;
            bit_send  <-- (consti 9 0);
            sm.set_next BIT_SEND_HIGH;
          ];
        ];

        BIT_SEND_HIGH, [
          data_reg <-- vdd;
          let delay = mux current_bit_is_high 
                      [consti 32 delay_0_high; consti 32 delay_1_high] in
          if_ (clk_count.value <: delay) [
            clk_count <-- clk_count.value +: 1;
          ] [
            clk_count <-- (consti 32 0);
            sm.set_next BIT_SEND_LOW;
          ];
        ];

        BIT_SEND_LOW, [
          data_reg <-- gnd;
          let delay = mux current_bit_is_high 
                        [consti 32 delay_0_low; consti 32 delay_1_low] in
          if_ (clk_count.value <: delay) [
            clk_count <-- clk_count.value +: 1;
          ] [
            clk_count <-- (consti 32 0);
            bit_send  <-- bit_send.value +: 1;
            sm.set_next DATA_SEND;
          ];
        ];
      ]
    ]);

  { O.data = data_reg.value }
end
