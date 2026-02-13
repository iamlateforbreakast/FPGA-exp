(* i2c_master.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; addr : 'a[@bits 7]
      ; reg_addr : 'a[@bits 8]
      ; din : 'a [@bits 8]
      ; rw : 'a
      ; start : 'a
      ; sda_in : 'a
      } 
      [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { scl : 'a
      ; sda_out : 'a
      ; ready : 'a
      ; ack_error : 'a
      ; dout : 'a [@bits 8]
      }
      [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
	let _sync_spec = Reg_spec.create ~clock:_input.clock ~reset:_input.reset () in

	(* --- Internal Signals & Registers --- *)
    let step_counter = reg_fb spec ~enable:vdd ~w:16 (fun fb -> fb +: 1) in
    let bit_index = Always.Variable.reg spec ~w:3 () in
    let shift_reg = Always.Variable.reg spec ~w:8 () in
    
    (* I2C Signals *)
    let scl_o = Always.Variable.all_reg spec ~w:1 () in
    let sda_o = Always.Variable.all_reg spec ~w:1 () in
    let sda_oe = Always.Variable.all_reg spec ~w:1 () in
    let ready = Always.Variable.all_reg spec ~w:1 () in
    let ack_err = Always.Variable.all_reg spec ~w:1 () in

	(* State machine *)
	let sm = Statemachine.create (module State) spec in

	sm.switch [
      IDLE, [
        ready <-- vdd;
        scl_o <-- vdd;
        sda_o <-- vdd;
        sda_oe <-- gnd;
        if_ input.start [
          shift_reg <-- input.addr @: input.rw;
          bit_index <-- of_int ~w:3 7;
          ready <-- gnd;
          step_counter <-- of_int ~w:16 0; (* Reset for START *)
          sm.set_next START;
        ];
      ];

      START, [
        sda_oe <-- vdd;
        sda_o <-- gnd; 
        if_ (step_counter.value ==: quarter_period) [
          scl_o <-- gnd;
          step_counter <-- of_int ~w:16 0; (* Reset for ADDR *)
          sm.set_next ADDR;
        ];
      ];

      ADDR, [
        sda_oe <-- vdd;
        sda_o <-- (shift_reg.value ==>: bit_index.value);
      
        if_ (step_counter.value ==: half_period) [ scl_o <-- vdd ];
      
        if_ (step_counter.value ==: full_period) [
          scl_o <-- gnd;
          step_counter <-- of_int ~w:16 0; (* Reset for next bit or state *)
          if_ (bit_index.value ==: zero 3) [
            sm.set_next ACK_ADDR;
          ] [
            bit_index <-- bit_index.value -: 1;
          ];
        ];
      ];

      ACK_ADDR, [
        sda_oe <-- gnd; 
        if_ (step_counter.value ==: half_period) [ scl_o <-- vdd ];
        if_ (step_counter.value ==: (quarter_period *: of_int ~w:16 3)) [
          ack_err <-- input.sda_in; 
        ];
        if_ (step_counter.value ==: full_period) [
          scl_o <-- gnd;
          step_counter <-- of_int ~w:16 0; (* Reset for STOP *)
          sm.set_next STOP;
        ];
      ];

      STOP, [
        sda_oe <-- vdd;
        sda_o <-- gnd;
        if_ (step_counter.value ==: quarter_period) [ scl_o <-- vdd ];
        if_ (step_counter.value ==: half_period) [ 
          sda_o <-- vdd;
          ready <-- vdd;
          sm.set_next IDLE;
        ];
      ];
    ]
  ]);
    (* Return circuit output value *)
    { O.scl = scl_o.value; O.sda_out = sda_o.value; O.ready = ready.value; 
      O.ack_error = ack_err.value; O.dout = zero 8 }
    	
  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"i2c_master" ~instance:"inst1" create i
    
end

(* Inside your create function 
let sda_i = wire 1 in
let sda_o = Always.Variable.wire ~default:gnd in
let sda_oe = Always.Variable.wire ~default:gnd in

(* AI *)
open Hardcaml
open Signal

module Make (X : Config.S) = struct
  (* ... Interfaces I and O as defined in your snippet ... *)

  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in

    (* --- Internal Signals & Registers --- *)
    let step_counter = reg_fb spec ~enable:vdd ~w:16 (fun fb -> fb +: 1) in
    let bit_index = Always.Variable.reg spec ~w:3 () in
    let shift_reg = Always.Variable.reg spec ~w:8 () in
    
    (* I2C Signals *)
    let scl_o = Always.Variable.all_reg spec ~w:1 () in
    let sda_o = Always.Variable.all_reg spec ~w:1 () in
    let sda_oe = Always.Variable.all_reg spec ~w:1 () in
    let ready = Always.Variable.all_reg spec ~w:1 () in
    let ack_err = Always.Variable.all_reg spec ~w:1 () in

    module State = struct
      type t = IDLE | START | ADDR | ACK_ADDR | STOP [@@deriving sexp_of, compare, enumerate]
    end
    let sm = Statemachine.create (module State) spec in

    (* Constant for timing: adjust based on clk frequency *)
    let quarter_period = of_int ~w:16 250 in 

    Always.(compile [
      sm.switch [
        IDLE, [
          ready <-- vdd;
          scl_o <-- vdd;
          sda_o <-- vdd;
          sda_oe <-- gnd;
          if_ input.start [
            shift_reg <-- input.addr @: input.rw; (* Concatenate addr + rw *)
            bit_index <-- of_int ~w:3 7;
            ready <-- gnd;
            sm.set_next START;
          ];
        ];

        START, [
          sda_oe <-- vdd;
          sda_o <-- gnd; (* SDA drop while SCL high *)
          if_ (step_counter.value ==: quarter_period) [
            scl_o <-- gnd;
            sm.set_next ADDR;
          ];
        ];

        ADDR, [
          sda_oe <-- vdd;
          sda_o <-- (shift_reg.value ==>: bit_index.value);
          
          (* Simple SCL toggling logic *)
          if_ (step_counter.value ==: (quarter_period *: of_int ~w:16 2)) [ scl_o <-- vdd ];
          if_ (step_counter.value ==: (quarter_period *: of_int ~w:16 4)) [
            scl_o <-- gnd;
            if_ (bit_index.value ==: zero 3) [
              sm.set_next ACK_ADDR;
            ] [
              bit_index <-- bit_index.value -: 1;
            ];
          ];
        ];

        ACK_ADDR, [
          sda_oe <-- gnd; (* Release bus for slave ACK *)
          if_ (step_counter.value ==: (quarter_period *: of_int ~w:16 2)) [
            scl_o <-- vdd;
          ];
          if_ (step_counter.value ==: (quarter_period *: of_int ~w:16 3)) [
            (* Sample ACK: SDA should be LOW from slave *)
            ack_err <-- input.sda_in; 
          ];
          if_ (step_counter.value ==: (quarter_period *: of_int ~w:16 4)) [
            scl_o <-- gnd;
            sm.set_next STOP;
          ];
        ];

        STOP, [
          sda_oe <-- vdd;
          sda_o <-- gnd;
          if_ (step_counter.value ==: quarter_period) [ scl_o <-- vdd ];
          if_ (step_counter.value ==: (quarter_period *: of_int ~w:16 2)) [ 
            sda_o <-- vdd; (* SDA rise while SCL high *)
            sm.set_next IDLE;
          ];
        ];
      ]
    ]);

    { O.scl = scl_o.value; O.sda_out = sda_o.value; O.ready = ready.value; 
      O.ack_error = ack_err.value; O.dout = zero 8 }
end

*)
