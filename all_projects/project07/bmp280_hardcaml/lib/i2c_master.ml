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
	    ; sda_oe : 'a
      ; ready : 'a
      ; ack_error : 'a
      ; dout : 'a [@bits 8]
      }
      [@@deriving hardcaml]
  end

  module State = struct
    type t = IDLE | START | ADDR | ACK_ADDR | REG_ADDR | ACK_REG 
         | RESTART | ADDR_READ | ACK_ADDR_READ | READ_DATA | WRITE_DATA 
         | ACK_WRITE | MSTR_ACK | STOP 
         [@@deriving sexp_of, compare, enumerate]
  end
  
  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let quarter_period = of_int ~width:16 67 in (* 100 kHz *)
    let half_period = quarter_period *: of_int ~width:16 2 in
    let full_period = quarter_period *: of_int ~width:16 4 in
	
	  let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in

	  (* --- Internal Signals & Registers --- *)
    let step_counter = Always.Variable.reg sync_spec ~enable:vdd ~width:16 in
    let bit_index = Always.Variable.reg sync_spec ~enable:vdd ~width:3 in
    let shift_reg = Always.Variable.reg sync_spec ~enable:vdd ~width:8 in
    
    (* I2C Signals *)
    let scl_o = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in
    let sda_o = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in
    let sda_oe = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in
    let ready = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in
    let ack_err = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in

	  (* State machine *)
	  let sm = Always.State_machine.create (module State) sync_spec in

    (* State Machine Logic *)
    Always.(compile [
      step_counter <-- step_counter.value +:. 1;
      sm.switch [
        IDLE, [
          ready <-- vdd;
          scl_o <-- vdd;
          sda_o <-- vdd;
          sda_oe <-- gnd;
          if_ input.start [
            shift_reg <-- input.addr @: input.rw;
            bit_index <-- of_int ~width:3 7;
            ready <-- gnd;
            step_counter <-- zero 16; (* Reset for START *)
            sm.set_next START;
          ][];
        ];

        START, [
          sda_oe <-- vdd;
          sda_o <-- gnd; 
          if_ (step_counter.value ==: quarter_period) [
            scl_o <-- gnd;
            step_counter <-- zero 16; (* Reset for ADDR *)
            sm.set_next ADDR;
          ][];
        ];

        ADDR, [
          sda_oe <-- vdd;
          sda_o <-- (bit shift_reg.value (to_int bit_index.value));
        
          if_ (step_counter.value ==: half_period) [ scl_o <-- vdd ][];
        
          if_ (step_counter.value ==: full_period) [
            scl_o <-- gnd;
            step_counter <-- zero 16; (* Reset for next bit or state *)
            if_ (bit_index.value ==: zero 3) [
              sm.set_next ACK_ADDR;
            ] [
              bit_index <-- bit_index.value -:. 1;
            ];
          ][];
        ];

        ACK_ADDR, [
          sda_oe <-- gnd;
          if_ (step_counter.value ==: half_period) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: full_period) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            (* If we just sent the Slave Addr, move to Reg Addr *)
            shift_reg <-- input.reg_addr;
            bit_index <-- of_int ~width:3 7;
            sm.set_next REG_ADDR;
          ][];
        ];

        STOP, [
          sda_oe <-- vdd;
          sda_o <-- gnd;
          if_ (step_counter.value ==: quarter_period) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: half_period) [ 
            sda_o <-- vdd;
            ready <-- vdd;
            sm.set_next IDLE;
          ][];
        ];

        REG_ADDR, [
          sda_oe <-- vdd;
          sda_o <-- (bit  bit_index.value (to_int shift_reg.value));
          if_ (step_counter.value ==: half_period) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: full_period) [
          scl_o <-- gnd;
          step_counter <-- zero 16;
          if_ (bit_index.value ==: zero 3) [ sm.set_next ACK_REG ]
          [ bit_index <-- bit_index.value -:. 1 ];
          ][];
        ];

        ACK_REG, [
          sda_oe <-- gnd;
          if_ (step_counter.value ==: half_period) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: full_period) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            (* Decide: Write data or Restart for a Read? *)
            if_ input.rw [
              sm.set_next RESTART;
            ] [
              shift_reg <-- input.din;
              bit_index <-- of_int ~width:3 7;
              sm.set_next WRITE_DATA; (* You can implement this similarly to REG_ADDR *)
            ];
          ][];
        ];
        
        WRITE_DATA, [
          sda_oe <-- vdd;
          sda_o <-- (bit shift_reg.value (to_int bit_index.value));
          if_ (step_counter.value ==: half_period) [ scl_o <-- vdd ] [];
          if_ (step_counter.value ==: full_period) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            if_ (bit_index.value ==: zero 3) [ sm.set_next ACK_WRITE ]
            [ bit_index <-- bit_index.value -:. 1 ];
          ] [];
        ];

        ACK_WRITE, [
          sda_oe <-- gnd;
          if_ (step_counter.value ==: half_period) [ scl_o <-- vdd ] [];
          if_ (step_counter.value ==: full_period) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            sm.set_next STOP;
          ] [];
        ];

        RESTART, [
          (* Repeated Start: SDA goes high then low while SCL is high *)
          sda_oe <-- vdd;
          if_ (step_counter.value ==: zero 16) [ sda_o <-- vdd ][];
          if_ (step_counter.value ==: quarter_period) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: half_period) [ sda_o <-- gnd ][]; (* Start condition *)
          if_ (step_counter.value ==: (quarter_period *: (of_int ~width:16 3))) [
            scl_o <-- gnd;
            shift_reg <-- input.addr @: vdd; (* Address + READ bit *)
            bit_index <-- of_int ~width:3 7;
            step_counter <-- zero 16  ;
            sm.set_next ADDR_READ; (* New state to handle the second address phase *)
          ][];
        ];

        READ_DATA, [
          sda_oe <-- gnd; (* Release SDA for slave to drive data *)
          if_ (step_counter.value ==: half_period) [
            scl_o <-- vdd;
            (* Sample data at the middle of the high SCL pulse *)
            shift_reg <-- insert ~into:shift_reg.value ~at_offset:(to_int bit_index.value) input.sda_in;
          ][];
          if_ (step_counter.value ==: full_period) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            if_ (bit_index.value ==: zero 3) [ sm.set_next MSTR_ACK ]
            [ bit_index <-- bit_index.value -:. 1 ];
          ][];
        ];

        MSTR_ACK, [
          sda_oe <-- vdd;
          (* For a single byte read, send NACK (SDA High) to end *)
          sda_o <-- vdd; 
          if_ (step_counter.value ==: half_period) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: full_period) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            sm.set_next STOP;
          ][];
        ];
      ]
    ]);
    
    (* Return circuit output value *)
    { O.scl = scl_o.value
    ; O.sda_out = sda_o.value
    ; O.sda_oe = sda_oe.value
    ; O.ready = ready.value
    ; O.ack_error = ack_err.value
    ; O.dout = shift_reg.value }
    	
  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
      H.hierarchical ~scope ~name:"i2c_master" ~instance:"inst1" create i
    
end
