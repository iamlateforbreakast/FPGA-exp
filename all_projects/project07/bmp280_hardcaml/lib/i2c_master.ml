(* i2c_master.ml *)
open Hardcaml
open Signal

(* TODO 1: Bit Ordering & Shifting: In SET_ADDR, you check if bit_index == 0 but perform the shift/assignment after the check. 
   This will likely result in the first bit being missed or the 8th bit being sent incorrectly. Additionally, sda_o <-- (bit 
   shift_reg.value 7) is inside the counter block, but shift_reg is shifted simultaneously; usually, you want to drive SDA 
   from the MSB before the first clock edge. *)
(* TODO 2: Sampling Logic: In READ_DATA, you are shifting shift_reg on every clock cycle where step_counter == quarter_period * 2.
   This means you are shifting 8 times per bit if your counter logic isn't perfectly gated, or you are shifting the wrong values. *)
(* TODO 3: *)
module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; dev_addr : 'a[@bits 7]
      ; reg_addr : 'a[@bits 8]
      ; mosi : 'a [@bits 8]
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
      ; miso : 'a [@bits 48]
      }
      [@@deriving hardcaml]
  end

  module State = struct
    type t = IDLE | START | SET_ADDR | WAIT_ACK_ADDR | SET_REG | WAIT_ACK_REG 
         | RESTART | SET_ADDR_READ | READ_DATA | MSTR_ACK | STOP 
         [@@deriving sexp_of, compare, enumerate]
  end
  
  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let quarter_period = if (X.is_simulation = false) then 67 else 3 in (* 100 kHz *)
	
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
    let byte_count = Always.VAriable.reg sync_spec ~enable:vdd ~width:8 in
    (* State machine *)
    let sm = Always.State_machine.create (module State) sync_spec in

    (* Debug *)
    let _ = Signal.(scl_o.value -- "scl_o") in
    let _ = Signal.(sda_o.value -- "sda_o") in
    let _ = Signal.(sda_oe.value -- "sda_oe") in
    let _ = Signal.(ready.value -- "ready") in
    let _ = Signal.(ack_err.value -- "ack_error") in
    let _ = Signal.(step_counter.value -- "step_counter") in
    let _ = Signal.(bit_index.value -- "bit_index") in
    let _ = Signal.(shift_reg.value -- "shift_reg") in
    let _ = Signal.(sm.current -- "state") in

    (* State Machine Logic *)
    Always.(compile [
      step_counter <-- step_counter.value +:. 1;
      sm.switch [
        IDLE, [
          ack_err <-- gnd;
          ready <-- vdd;
          scl_o <-- vdd;
          sda_o <-- vdd;
          sda_oe <-- gnd;
          if_ input.start [
            shift_reg <-- input.dev_addr @: (zero 1);
            bit_index <-- of_int ~width:3 7;
            step_counter <-- zero 16;
			byte_count <--. 6; (* Read 6 bytes of measurements *)
            sm.set_next START;
          ][];
        ];

        START, [
          sda_oe <-- vdd;
          sda_o <-- gnd;
          ready <-- gnd;
          if_ (step_counter.value ==: (of_int ~width:16 quarter_period)) [
            scl_o <-- gnd;
            step_counter <-- zero 16; (* Reset for ADDR *)
            sm.set_next SET_ADDR;
          ][];
        ];

        SET_ADDR, [
          sda_oe <-- vdd;
        
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 2))) [ scl_o <-- vdd ][];
        
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 4))) [
            scl_o <-- gnd;
            step_counter <-- zero 16; (* Reset for next bit or state *)
            if_ (bit_index.value ==: zero 3) [
              sm.set_next WAIT_ACK_ADDR;
            ] [
              bit_index <-- bit_index.value -:. 1;
              shift_reg <-- sll shift_reg.value 1;
              sda_o <-- (bit shift_reg.value 7);
            ];
          ][];
        ];

        WAIT_ACK_ADDR, [
          sda_oe <-- gnd;
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 2))) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 4))) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            (* If we just sent the Slave Addr, move to Reg Addr *)
            shift_reg <-- input.reg_addr;
            bit_index <-- of_int ~width:3 7;
            sm.set_next SET_REG;
          ][];
        ];

        STOP, [
          sda_oe <-- vdd;
          sda_o <-- gnd;
          if_ (step_counter.value ==: (of_int ~width:16 quarter_period)) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 2))) [ 
            sda_o <-- vdd;
            ready <-- vdd;
            sm.set_next IDLE;
          ][];
        ];

        SET_REG, [
          sda_oe <-- vdd;
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 2))) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 4))) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            if_ (bit_index.value ==: zero 3) [
			        sm.set_next WAIT_ACK_REG
			      ] [
			        bit_index <-- bit_index.value -:. 1;
              shift_reg <-- sll shift_reg.value 1;
              sda_o <-- (bit shift_reg.value 7);
			      ];
          ][];
        ];

        WAIT_ACK_REG, [
          sda_oe <-- gnd;
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 2))) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 4))) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            (* Decide: Write data or Restart for a Read? *)
            if_ input.rw [
              sm.set_next RESTART;
            ] [
              shift_reg <-- input.mosi;
              bit_index <-- of_int ~width:3 7;
              sm.set_next RESTART; (* You can implement this similarly to REG_ADDR *)
            ];
          ][];
        ];
        
        RESTART, [
          (* Repeated Start: SDA goes high then low while SCL is high *)
          sda_oe <-- vdd;
          if_ (step_counter.value ==: zero 16) [ sda_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:16 quarter_period)) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 2))) [ sda_o <-- gnd ][]; (* Start condition *)
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 3))) [
            scl_o <-- gnd;
            shift_reg <-- input.dev_addr @: vdd; (* Address + READ bit *)
            bit_index <-- of_int ~width:3 7;
            step_counter <-- zero 16  ;
            sm.set_next SET_ADDR_READ; (* New state to handle the second address phase *)
          ][];
        ];

        SET_ADDR_READ, [
          sda_oe <-- vdd;
          sda_o <-- (bit shift_reg.value 7);
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 2))) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 4))) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            shift_reg <-- sll shift_reg.value 1;
            if_ (bit_index.value ==: zero 3) [
			  byte_count <--. 10;
              sm.set_next READ_DATA; (* After addr ack, go to read *)
            ] [
              bit_index <-- bit_index.value -:. 1;
            ];
          ][];
		    ];
		
        READ_DATA, [
          sda_oe <-- gnd; (* Release SDA for slave to drive data *)
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 2))) [
            scl_o <-- vdd;
            (* Sample data at the middle of the high SCL pulse *)
            (* TBC: shift_reg <-- insert ~into:shift_reg.value ~at_offset:(to_int bit_index.value) input.sda_in; *)
            shift_reg <-- ((sll shift_reg.value 1) |: (zero 7 @: input.sda_in));
          ][];
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 4))) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
            if_ (bit_index.value ==: zero 3) [
			  byte_count <-- byte_count.value -:. 1;
			  sm.set_next MSTR_ACK;
			] [
			  bit_index <-- bit_index.value -:. 1 
			];
          ][];
        ];

        MSTR_ACK, [
          sda_oe <-- vdd;
          (* For a single byte read, send NACK (SDA High) to end *)
		  if_ (byte_count.value ==: 0) [
		    sda_o <-- vdd; (* NACK: End of acquisition *)
			sm.set_next STOP;
		  ][
		    sda_o <-- gnd; (* ACK: More bytes to read  *)
			sm.set_next READ_DATA;
		  ]
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 2))) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:16 (quarter_period * 4))) [
            scl_o <-- gnd;
            step_counter <-- zero 16;
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
    ; O.miso = shift_reg.value }
    	
  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
      H.hierarchical ~scope ~name:"i2c_master" ~instance:"inst1" create i
    
end
