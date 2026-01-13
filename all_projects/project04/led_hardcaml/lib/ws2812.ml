(* ws2812.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
  type 'a t = {
    clock   : 'a;
    reset : 'a;
    color : 'a; [@bits 24]
  } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      data : 'a;
    } [@@deriving hardcaml]
  end

  module State = struct
    type t = IDLE | DATA_SEND | BIT_SEND_HIGH | BIT_SEND_LOW
    [@@deriving sexp_of, enumerate, compare]
  end
  
  let create (_scope: Scope.t) (input: Signal.t I.t) : Signal.t O.t =
    let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in
    let sm = Always.State_machine.create(module State) sync_spec in
    
    (* Constants calculations *)
    let f = float_of_int X.clk_fre /. 1_000_000. in
    let delay_1_high = Int.of_float (f *. 0.85) - 1 in
    let delay_1_low  = Int.of_float (f *. 0.40) - 1 in
    let delay_0_high = Int.of_float (f *. 0.40) - 1 in
    let delay_0_low  = Int.of_float (f *. 0.85) - 1 in
    let delay_reset  = (X.clk_fre / 10) - 1 in

    (* Registers *)
    let bit_send    = Always.Variable.reg sync_spec ~width:9 in
    let data_send   = Always.Variable.reg sync_spec ~width:9 in
    let clk_count   = Always.Variable.reg sync_spec ~width:32 in
    let data_reg    = Always.Variable.reg sync_spec ~width:1 in
    let ws_data     = Always.Variable.reg sync_spec ~width:24 in
    let ws_valid    = Always.Variable.reg sync_spec ~width:1 in

    (* Helper to index the data bit *)
    let current_bit_is_high = mux bit_send.value (bits_msb ws_data.value) in

    (* Logic Description *)
    Always.(compile [
      sm.switch [
        IDLE, [
          data_reg <-- gnd;
          if_ (clk_count.value <:. delay_reset) [
            clk_count <-- clk_count.value +:. 1;
          ] [
            clk_count <-- zero 32;
            if_ (ws_data.value <>: input.color |: ~:(ws_valid.value)) [
              ws_valid <-- vdd;
              ws_data  <-- input.color;
              sm.set_next DATA_SEND;
            ] [];
          ];
        ];

        DATA_SEND, [
          if_ ((data_send.value >:. X.ws2812_num) &: 
            (bit_send.value ==:. X.ws2812_width)) [
              clk_count <-- zero 32;
              data_send <-- zero 9;
              bit_send  <-- zero 9;
              sm.set_next IDLE;
              ] @@ elif (bit_send.value <:. X.ws2812_width) [
                sm.set_next BIT_SEND_HIGH;
              ] [
                data_send <-- data_send.value +:. 1;
                bit_send  <-- zero 9;
                sm.set_next BIT_SEND_HIGH;
              ];
        ];

        BIT_SEND_HIGH, [
          data_reg <-- vdd;
          let delay = mux current_bit_is_high 
                        [of_int ~width:32 delay_0_high; of_int ~width:32 delay_1_high] in
          if_ (clk_count.value <: delay) [
            clk_count <-- clk_count.value +:. 1;
          ] [
            clk_count <-- zero 32;
            sm.set_next BIT_SEND_LOW;
          ];
        ];

        BIT_SEND_LOW, [
          data_reg <-- gnd;
          let delay = mux current_bit_is_high 
                        [of_int ~width:32 delay_0_low; of_int ~width:32 delay_1_low] in
          if_ (clk_count.value <: delay) [
            clk_count <-- clk_count.value +:. 1;
          ] [
            clk_count <-- zero 32;
            bit_send  <-- bit_send.value +:. 1;
            sm.set_next DATA_SEND;
          ];
        ];
      ]
    ]);
    { O.data = data_reg.value }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"ws2812" ~instance:"inst1" create i

end
