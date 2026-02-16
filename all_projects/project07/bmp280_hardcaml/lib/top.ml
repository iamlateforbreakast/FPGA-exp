(* top.ml *)
open Hardcaml
open Signal
open Base

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a [@rtlname "I_clk"]
      ; reset : 'a [@rtlname "I_rst"]
      ; sda_in : 'a [@rtlname "I_sda_in"]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds : 'a[@bits 6]
      ; scl : 'a [@rtlname "O_scl"]
      ; sda_out : 'a [@rtlname "O_sda_out"]
      ; sda_oe : 'a [@rtlname "O_sda_oe"]
      ; uart_tx : 'a [@rtlname "O_uart_tx"]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = INIT | SEND_CONFIG | WAIT_CONFIG | READ_DATA | WAIT_DATA
    [@@deriving sexp_of, compare, enumerate]
  end

  (* Create configured modules *)
  module MyI2c_master = I2c_master.Make(X)
  module MyLeds = Leds.Make(X)

  (* Create GOWIN primitive components *)
  let iobuf ~din ~oen =
    let m = Instantiation.create
      ~name:"IOBUF" (* Gowin primitive name *)
      ~inputs:[ "I", din
              ; "OEN", oen
              ]
      ~outputs:[ "IO", 1
               ; "O", 1 ]
               ()
    in (Map.find_exn m "IO" -- "sda_io", 
        Map.find_exn m "O"  -- "sda_from_bus")
	  
  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let open Always in
	  let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in

    let sm = State_machine.create (module States) sync_spec ~enable:vdd in

    let start = Variable.reg ~enable:vdd sync_spec ~width:1 in
    let reg_addr = Variable.reg ~enable:vdd sync_spec ~width:8 in
    let wdata = Variable.reg ~enable:vdd sync_spec ~width:8 in

    let i2c_master = MyI2c_master.hierarchical scope (
	     MyI2c_master.I.{ reset=input.reset
                      ; clock=input.clock
                      ; dev_addr=(of_int ~width:7 X.i2c_address)
                      ; reg_addr=reg_addr.value
                      ; mosi=wdata.value
                      ; rw=gnd  (* 1 for write, 0 for read *)
                      ; start = start.value
                      ; sda_in = input.sda_in }) in

    let leds = MyLeds.hierarchical scope (
	     MyLeds.I.{ reset=input.reset; clock=input.clock }) in

	  (* Instantiate the IOBUF for SDA *)
    let (_sda_io,_sda_from_bus) = iobuf ~din:i2c_master.sda_out ~oen:i2c_master.sda_oe in

    Always.(compile [
      sm.switch [
        States.INIT, [
          start    <--. 0;
		  sm.set_next SEND_CONFIG;
        ];
        States.SEND_CONFIG, [
		  reg_addr <--. 0xF4; (* Temperature *)
          reg_addr <--. 0xF7;  (* Pressure MSB *)
          start    <--. 1;
		  sm.set_next WAIT_CONFIG;
        ];
		States.WAIT_CONFIG [
		  if_ i2c_master.ready [
            sm.set_next States.READ_DATA;
          ][]
		];
        States.READ_DATA [
		];
		States.WAIT_DATA [
		];
      ]
    ];);

    (* Return circuit output value *)
    { O.leds = (~:(leds.leds))
    ; O.scl = gnd (* i2c_master.scl *) 
    ; O.sda_out = gnd (* i2c_master.sda_out a*)
    ; O.sda_oe = gnd (* i2c_master.sda_oe *)
    ; O.uart_tx = gnd
    } 

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
    
end
