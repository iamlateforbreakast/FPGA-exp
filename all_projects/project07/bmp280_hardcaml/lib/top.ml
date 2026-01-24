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
      { clock : 'a
      ; reset : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds : 'a[@bits 6]
      ; i2c : 'a
      }
    [@@deriving hardcaml]
  end

  module MyI2c_master = I2c_master.Make(X)
	
  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let open Always in
	  let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in

    (*
    reg [3:0] state;
    reg start;
    wire done;
    reg [7:0] reg_addr, wdata;
    *)
    let state = Variable.reg ~enable:vdd sync_spec ~width:4 in
    let start = Variable.reg ~enable:vdd sync_spec ~width:1 in
    let reg_addr = Variable.reg ~enable:vdd sync_spec ~width:8 in
    let wdata = Variable.reg ~enable:vdd sync_spec ~width:8 in

    let i2c_master = MyI2c_master.hierarchical _scope (
	     MyI2c_master.I.{ reset=input.reset; clock=input.clock }) in

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
    { O.leds = zero 6; O.i2c_master = i2c_master.data }  (* Placeholder for actual LED output *)

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
    
end
