(* screen_spi.ml *)
open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

module Make (X : Config.S) = struct
  module I = struct
    type 'a t =
      { clock   : 'a
      ; reset : 'a
      ; data_in : 'a [@bits 8]
      ; data_valid : 'a
      } 
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { mosi : 'a
      ; sclk : 'a
      ; cs :   'a
      ; ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in
  
    (* Define a divider constant in your Config *)
    let divider_limit = 5 in

    (* Inside create function *)
    let clk_cnt = Always.Variable.reg spec ~width:8 in
    let clk_tick = Always.Variable.wire ~default:(zero 1) in

    (* Internal Registers *)
    let shift_reg = Always.Variable.reg spec ~width:8 in
    let bit_cnt   = Always.Variable.reg spec ~width:4 in
    let running   = Always.Variable.reg spec ~width:1 in
    let sclk_reg  = Always.Variable.reg spec ~width:1 in

    let _ = Signal.(sclk_reg.value -- "sclk_reg") in
    let _ = Signal.(running.value -- "running") in
    let _ = Signal.(bit_cnt.value -- "bit_cnt") in
    let _ = Signal.(shift_reg.value -- "shift_reg") in
    let _ = (input.data_in -- "data_in") in
    let _ = (input.data_valid -- "data_valid") in

    Always.(compile [
      if_ (clk_cnt.value ==:. divider_limit - 1) [
        clk_cnt <--. 0;
      ] [
        clk_cnt <-- clk_cnt.value +:. 1;
      ];
      clk_tick <-- (clk_cnt.value ==:. 0);
      if_ (input.data_valid &: ~:(running.value) ) [
        shift_reg <-- input.data_in;
        bit_cnt   <--. 0;
        running   <-- vdd;
        sclk_reg  <-- gnd;
      ] [
          if_ (running.value &: clk_tick.value) [
            (* Toggle clock and shift logic *)
            sclk_reg <-- ~:(sclk_reg.value);
            if_ (sclk_reg.value) [ (* Falling edge of SCLK *)
              shift_reg <-- (sll shift_reg.value 1); (* Shift Left *)
              bit_cnt   <-- (bit_cnt.value +:. 1);][];
          
            if_ (bit_cnt.value ==:. 8) [
              running <-- gnd;
            ] [];
          ] [];
      ]];);
    { O.mosi = msb shift_reg.value
    ; sclk = sclk_reg.value
    ; cs = ~:(running.value)
    ; ready = ~:(running.value)
    }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"screen_spi" ~instance:"inst1" create i
end
