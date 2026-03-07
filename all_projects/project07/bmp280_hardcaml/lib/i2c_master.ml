(* i2c_master.ml *)
open Hardcaml
open Signal

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
      ; miso : 'a [@bits 48] (* 6 bytes read from BMP280 *)
      }
      [@@deriving hardcaml]
  end

  module State = struct
    type t = IDLE | START | SET_ADDR | WAIT_ACK_ADDR | SET_REG | WAIT_ACK_REG
	     | SET_REG_DATA | WAIT_ACK_REG_DATA
         | RESTART | SET_ADDR_READ | WAIT_ACK_READ_ADDR | READ_DATA | MSTR_ACK | STOP 
         [@@deriving sexp_of, compare, enumerate]
  end
  
  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    (* --- Constants --- *)
    let quarter_period = if (X.is_simulation = false) then 67 else 3 in (* 100 kHz *)
	
    let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in

    let cycle = Signal.reg_fb sync_spec ~enable:vdd  ~width:16
                       ~f:(fun d -> mux2 (d <: of_int ~width:16 quarter_period) (d +:. 1) (zero 16)) in

	(* --- Internal Signals & Registers --- *)
    let step_counter = Always.Variable.reg sync_spec ~enable:vdd ~width:8 in
    let bit_index = Always.Variable.reg sync_spec ~enable:vdd ~width:3 in
    let shift_reg = Always.Variable.reg sync_spec ~enable:vdd ~width:8 in
    let miso_buffer = Always.Variable.reg sync_spec ~enable:vdd ~width:48 in
	
    (* --- I2C Signals --- *)
    let scl_o = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in
    let sda_o = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in
    let sda_oe = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in
    let ready = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in
    let ack_err = Always.Variable.reg sync_spec ~enable:vdd ~width:1 in
    let byte_count = Always.Variable.reg sync_spec ~enable:vdd ~width:8 in
	
    (* --- State machine --- *)
    let sm = Always.State_machine.create (module State) sync_spec in

    (* I2C Clock generation*)
    let _start_of_bit = (cycle ==: zero 16) in
    let _rising_edge = (cycle ==: of_int ~width:16 (quarter_period - 1)) in
    let _middle_of_bit = (cycle ==: of_int ~width:16 (quarter_period * 2 - 1)) in
    let _falling_edge = (cycle ==: of_int ~width:16 (quarter_period * 3 - 1)) in
    let _end_of_bit = (cycle ==: of_int ~width:16 (quarter_period * 4 - 1)) in

    (* --- Debug --- *)
    let _ = Signal.(scl_o.value -- "scl_o") in
    let _ = Signal.(sda_o.value -- "sda_o") in
    let _ = Signal.(sda_oe.value -- "sda_oe") in
    let _ = Signal.(ready.value -- "ready") in
    let _ = Signal.(ack_err.value -- "ack_error") in
    let _ = Signal.(step_counter.value -- "step_counter") in
    let _ = Signal.(bit_index.value -- "bit_index") in
    let _ = Signal.(shift_reg.value -- "shift_reg") in
    let _ = Signal.(byte_count.value -- "byte_count") in
    let _ = Signal.(sm.current -- "state") in
    let _ = Signal.(cycle -- "cycle") in
    
    (* --- State Machine Logic --- *)
    Always.(compile [
      step_counter <-- step_counter.value +:. 1;
      sm.switch [
        IDLE, [
          ack_err <-- gnd;
          ready <-- vdd;
          scl_o <-- vdd;
          sda_o <-- vdd;
          sda_oe <-- vdd;
		      miso_buffer <--. 0;
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            if_ input.start [
              ready <-- gnd;
              shift_reg <-- input.dev_addr @: (zero 1);
              bit_index <-- of_int ~width:3 7;
              step_counter <-- zero 8;
			        byte_count <--. 6; (* Read 6 bytes of measurements *)
              sda_oe <-- gnd; (* GND enables output *)
              sm.set_next START;
            ][];
          ][];
        ];

        START, [
          sda_o <-- gnd;
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [scl_o <-- gnd;][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            step_counter <-- zero 8; (* Reset for ADDR *)
            sm.set_next SET_ADDR;
          ][];
        ];

        SET_ADDR, [
          if_ (step_counter.value ==: zero 8) [
            sda_o <-- (bit shift_reg.value 7);
          ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period))) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ scl_o <-- gnd; ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [       
            step_counter <-- zero 8; (* Reset for next bit or state *)
            shift_reg <-- sll shift_reg.value 1;
            if_ (bit_index.value ==: zero 3) [
              sda_oe <-- vdd; (* Release SDA to wait for ACK *)
              sm.set_next WAIT_ACK_ADDR;
            ] [
              bit_index <-- bit_index.value -:. 1;
            ];
          ][];
        ];

        WAIT_ACK_ADDR, [
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period))) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: of_int ~width:8 (quarter_period * 2)) [
            (* sample ACK *)
            ack_err <-- input.sda_in;  (* 0 = ACK, 1 = NACK *)
          ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ scl_o <-- gnd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            step_counter <-- zero 8;
            sda_oe <-- gnd;
            if_ ack_err.value [
              sm.set_next STOP;
            ][
              (* If we just sent the Slave Addr, move to Reg Addr *)
              shift_reg <-- input.reg_addr;
              bit_index <-- of_int ~width:3 7;
              sm.set_next SET_REG;
            ]
          ][];
        ];

        STOP, [
          sda_oe <-- gnd;
          sda_o <-- gnd;
          if_ (step_counter.value ==: (of_int ~width:8 quarter_period)) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 2))) [ 
            sda_o <-- vdd;
            ready <-- vdd;
            step_counter <-- zero 8;
            sm.set_next IDLE;
          ][];
        ];

        SET_REG, [
          if_ (step_counter.value ==: zero 8) [
            sda_o <-- (bit shift_reg.value 7);
          ][];
          if_ (step_counter.value ==: (of_int ~width:8 quarter_period )) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ scl_o <-- gnd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            step_counter <-- zero 8;
            shift_reg <-- sll shift_reg.value 1;
            if_ (bit_index.value ==: zero 3) [
              sda_oe <-- vdd;
			        sm.set_next WAIT_ACK_REG;
			      ] [
              bit_index <-- bit_index.value -:. 1;
			      ];
          ][];
        ];

        WAIT_ACK_REG, [
          if_ (step_counter.value ==: (of_int ~width:8 quarter_period)) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: of_int ~width:8 (quarter_period * 2)) [
            (* sample ACK *)
            ack_err <-- input.sda_in;  (* 0 = ACK, 1 = NACK *)
          ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ scl_o <-- gnd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            step_counter <-- zero 8;
            sda_oe <-- gnd;
            (* Decide: Write data or Restart for a Read? *)
            if_ ack_err.value [sm.set_next STOP;
            ][
              if_ input.rw [
                sm.set_next RESTART;
              ] [
                shift_reg <-- input.mosi;
                bit_index <-- of_int ~width:3 7;
                sm.set_next SET_REG_DATA;
              ];
            ];
          ][];
        ];

		SET_REG_DATA, [
          (* Drive SDA with the current MSB of shift_reg *)
          if_ (step_counter.value ==: zero 8) [
            sda_o <-- (bit shift_reg.value 7);
          ][];
          
          (* Clock high in the middle of the bit period *)
          if_ (step_counter.value ==: (of_int ~width:8 quarter_period)) [ 
            scl_o <-- vdd 
          ][];
          
          (* Clock low for data transition *)
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ 
            scl_o <-- gnd 
          ][];
          
          (* End of bit period: shift or change state *)
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            step_counter <-- zero 8;
            shift_reg <-- sll shift_reg.value 1;
            
            if_ (bit_index.value ==: zero 3) [
              sda_oe <-- vdd; (* Release SDA to wait for ACK *)
              sm.set_next WAIT_ACK_REG_DATA;
            ] [
              bit_index <-- bit_index.value -:. 1;
            ];
          ][];
        ];

		WAIT_ACK_REG_DATA, [
          (* Master releases SDA; Slave pulls it low to ACK *)
          if_ (step_counter.value ==: (of_int ~width:8 quarter_period)) [ 
            scl_o <-- vdd 
          ][];
          
          if_ (step_counter.value ==: of_int ~width:8 (quarter_period * 2)) [
            (* Sample ACK bit: 0 = SUCCESS, 1 = FAIL *)
            ack_err <-- input.sda_in;
          ][];
          
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ 
            scl_o <-- gnd 
          ][];
          
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            step_counter <-- zero 8;
            sda_oe <-- gnd; (* Re-enable master control of SDA *)
            
            (* For BMP280 writes, we usually stop after one byte or repeat *)
            sm.set_next STOP;
          ][];
        ];
		
        RESTART, [
          (* Repeated Start: SDA goes high then low while SCL is high *)
          if_ (step_counter.value ==: zero 8) [ sda_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:8 quarter_period)) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 2))) [ sda_o <-- gnd ][]; (* Start condition *)
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ scl_o <-- gnd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            shift_reg <-- input.dev_addr @: vdd; (* Address + READ bit *)
            bit_index <-- of_int ~width:3 7;
            sda_oe <-- gnd;
            step_counter <-- zero 8  ;
            sm.set_next SET_ADDR_READ; (* New state to handle the second address phase *)
          ][];
        ];

        SET_ADDR_READ, [
          sda_o <-- (bit shift_reg.value 7);
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period))) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ scl_o <-- gnd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            step_counter <-- zero 8;
            shift_reg <-- sll shift_reg.value 1;
            if_ (bit_index.value ==: zero 3) [
			        byte_count <--. 6;
              sda_oe <-- vdd; (* Release SDA for slave to drive data *)
              bit_index <-- of_int ~width:3 7;
              sm.set_next WAIT_ACK_READ_ADDR;
            ] [
              bit_index <-- bit_index.value -:. 1;
            ];
          ][];
		];
		
        WAIT_ACK_READ_ADDR, [
          if_ (step_counter.value ==: (of_int ~width:8 quarter_period)) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 2))) [
            (* sample ACK *)
            ack_err <-- input.sda_in;  (* 0 = ACK, 1 = NACK *)
          ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ scl_o <-- gnd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            step_counter <-- zero 8;
            sda_oe <-- vdd; (* Release SDA for slave to drive data *)
            if_ ack_err.value [sm.set_next STOP][sm.set_next READ_DATA] (* After addr ack, go to read *)
          ][];
        ];

        READ_DATA, [
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period))) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 2))) [
            (* Sample data at the middle of the high SCL pulse *)
            (* TBC: shift_reg <-- insert ~into:shift_reg.value ~at_offset:(to_int bit_index.value) input.sda_in; *)
            miso_buffer <-- ((sll miso_buffer.value 1) |: (zero 47 @: input.sda_in));
          ][];

          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [scl_o <-- gnd][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            scl_o <-- gnd;
            step_counter <-- zero 8;
            if_ (bit_index.value ==: zero 3) [
			        byte_count <-- byte_count.value -:. 1;
              sda_oe <-- gnd;
              sm.set_next MSTR_ACK;
			      ] [
			        bit_index <-- bit_index.value -:. 1 
			      ];
          ][];
        ];

        MSTR_ACK, [
          (* For a single byte read, send NACK (SDA High) to end *)
		      if_ (byte_count.value ==: (of_int ~width:8 0)) [
		        sda_o <-- vdd; (* NACK: End of acquisition *)
		      ][
		        sda_o <-- gnd; (* ACK: More bytes to read  *)
		      ];
          if_ (step_counter.value ==: (of_int ~width:8 quarter_period)) [ scl_o <-- vdd ][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 3))) [ scl_o <-- gnd][];
          if_ (step_counter.value ==: (of_int ~width:8 (quarter_period * 4 - 1))) [
            step_counter <-- zero 8;
            if_ (byte_count.value ==: (of_int ~width:8 0)) [
              sda_oe <-- gnd;
              sm.set_next STOP;
            ][
              sda_oe <-- vdd;
              bit_index <-- of_int ~width:3 7;
              sm.set_next READ_DATA;
            ]
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
    ; O.miso = miso_buffer.value }
    	
  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
      H.hierarchical ~scope ~name:"i2c_master" ~instance:"inst1" create i
    
end
