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
      } 
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { mosi : 'a
      ; sclk : 'a
      ; cs : 'a
      ; dc : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in
  
    (* Internal Registers *)
    let shift_reg = Always.Variable.reg spec ~width:8 in
    let bit_cnt   = Always.Variable.reg spec ~width:4 in
    let running   = Always.Variable.reg spec ~width:1 in
    let sclk_reg  = Always.Variable.reg spec ~width:1 in

    Always.(compile [
      if_ (~:(running.value) ) [
        shift_reg <-- input.data_in;
        bit_cnt   <--. 0;
        running   <-- gnd;
      ] [
          (* Toggle clock and shift logic *)
          sclk_reg <-- ~:(sclk_reg.value);
          if_ (sclk_reg.value) [ (* Falling edge of SCLK *)
              shift_reg <-- ((sll shift_reg.value 1) @: (zero 1)); (* Shift Left *)
              bit_cnt   <-- (bit_cnt.value +:. 1);][];
          
          if_ (bit_cnt.value ==:. 8) [
            running <-- vdd;
          ] [];
        ]
      ]);
    { O.mosi = msb shift_reg.value
    ; sclk = sclk_reg.value
    ; cs = ~:(running.value)
    ; dc = running.value }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"screen_spi" ~instance:"inst1" create i
end
