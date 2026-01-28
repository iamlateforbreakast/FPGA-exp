(* i2c_master.ml *)
open Hardcaml
open Signal
(*
    input clk, rst_n,
    input [6:0] addr, [7:0] reg_addr, [7:0] din,
    input rw, start,
    output reg [7:0] dout, output reg done, ack_error,
    inout sda, output scl
*)

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
	
  let create (_scope : Scope.t) (_input : Signal.t I.t) : Signal.t O.t =
	let _sync_spec = Reg_spec.create ~clock:_input.clock ~reset:_input.reset () in

    let sda_i = wire 1 in
    let sda_o = Always.Variable.wire ~default:gnd in
    let sda_oe = Always.Variable.wire ~default:gnd in

	(* Instantiate the IOBUF for SDA *)
    let iobuf_res = Instantiation.create ~name:"IOBUF" ~scope
      ~inputs:[("I", sda_o.value); ("OEN", ~: (sda_oe.value))]
      ~outputs:[("O", 1)] () in
    sda_i <== (Map.find_exn iobuf_res "O");
	
    (* Return circuit output value *)
    { O.scl = zero 1
    ; O.sda_out = zero 1
    ; O.ready = vdd
    ; O.ack_error = gnd
    ; O.dout = zero 8
    }
  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"i2c_master" ~instance:"inst1" create i
    
end

(* Inside your create function *)
let sda_i = wire 1 in
let sda_o = Always.Variable.wire ~default:gnd in
let sda_oe = Always.Variable.wire ~default:gnd in

(* Instantiate the IOBUF for SDA *)
let iobuf_res = Instantiation.create ~name:"IOBUF" ~scope
  ~inputs:[("I", sda_o.value); ("OEN", ~: (sda_oe.value))]
  ~outputs:[("O", 1)] () in
sda_i <== (Map.find_exn iobuf_res "O");

(* FSM State Example for Start Condition *)
(* SCL is High, SDA transitions High -> Low *)
Always.(compile [
  sm.switch [
    START, [
      sda_oe <-- vdd;
      sda_o  <-- gnd; (* Pull down SDA *)
      if_ (timer.value ==: half_period) [ sm.set_next SEND_ADDR ];
    ];
    (* ... more states for ADDR, RW, ACK, DATA ... *)
  ]
]);
