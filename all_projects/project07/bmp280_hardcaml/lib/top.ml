(* top.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  (*
      input clk, rst_n,
    output [19:0] raw_temp, raw_press,
    inout sda, output scl
  *)
  module I = struct
    type 'a t =
      { clock : 'a [@rtlname "I_clk"]
      ; reset : 'a [@rtlname "I_rst"]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds : 'a[@bits 6]
      ; scl : 'a [@rtlname "O_scl"]
      ; sda : 'a [@rtlname "O_sda"]
      ; output : 'a[@bits 20] [@rtlname "O_dout"]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = INIT | SEND_CONFIG | WAIT_CONFIG | READ_DATA | WAIT_DATA
    [@@deriving sexp_of, compare, enumerate]
  end

  (* Create configured modules *)
  module MyI2c_master = I2c_master.Make(X)

  (* Create GOWIN primitive components *)
  let iobuf ~din ~dout ~oen =
    Instantiation.create
      ~name:"IOBUF" (* Gowin primitive name *)
      ~inputs:[ "I", din
	          ; "OEN", oen
              ]
      ~outputs:[ "IO", 1
		       ; "O", 1 ]
	  ()
      in (Map.find_exn outputs "IO", Map.find outputs "O")
	  
  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let open Always in
	  let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in

    let sm = State_machine.create (module States) sync_spec ~enable:vdd in
    (*
    reg [3:0] state;
    reg start;
    wire done;
    reg [7:0] reg_addr, wdata;
    *)
    let start = Variable.reg ~enable:vdd sync_spec ~width:1 in
    let reg_addr = Variable.reg ~enable:vdd sync_spec ~width:8 in
    let wdata = Variable.reg ~enable:vdd sync_spec ~width:8 in

    let i2c_master = MyI2c_master.hierarchical _scope (
	     MyI2c_master.I.{ reset=input.reset
                      ; clock=input.clock
                      ; addr=of_int ~width:7 X.i2c_address
                      ; reg_addr=reg_addr.value
                      ; din=wdata.value
                      ; rw=one 1  (* 1 for write, 0 for read *)
                      ; start=start.value 
                      ; sda_in = zero 1 }) in
					  
    let sda_i = wire 1 in
    let sda_o = Always.Variable.wire ~default:gnd in
    let sda_oe = Always.Variable.wire ~default:gnd in

	(* Instantiate the IOBUF for SDA *)
    let iobuf_res = Instantiation.create ~name:"IOBUF" ~scope
      ~inputs:[("I", sda_o.value); ("OEN", ~: (sda_oe.value))]
      ~outputs:[("O", 1)] () in
    sda_i <== (Map.find_exn iobuf_res "O");
	
    compile [
      sm.switch [
        States.INIT, [
          reg_addr <--. 0xF4;
          wdata    <--. 0x27;
          start    <--. 1;
          if_ i2c_master.ready [
            sm.set_next States.SEND_CONFIG;
          ][]
        ];
        States.SEND_CONFIG, [
          reg_addr <--. 0xF7;  (* Pressure MSB *)
          start    <--. 1;
          if_ i2c_master.ready [
            sm.set_next States.READ_DATA;
          ][]
        ];
        (* Continue sequencing through all 6 data registers *)
      ]
    ];
    (*
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) state <= 0;
        else case (state)
            0: begin // Step 1: Configure Sensor
                reg_addr <= 8'hF4; wdata <= 8'h27; start <= 1;
                if (done) state <= 1;
            end
            1: begin // Step 2: Request Pressure MSB (0xF7)
                reg_addr <= 8'hF7; start <= 1;
                if (done) state <= 2;
            end
            // Continue sequencing through all 6 data registers
        endcase
    end
    *)

    (* Return circuit output value *)
    { O.leds = zero 6
    ; O.output = i2c_master.dout
    ; O.scl = i2c_master.scl 
    ; O.sda = i2c_master.sda_out
    } 

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
    
end
