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
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds    : 'a[@bits 6] [@rtlname "O_led"]
      ; scl     : 'a [@rtlname "O_scl"]
      ; sda_out : 'a [@rtlname "O_sda_out"]
      ; uart_tx : 'a [@rtlname "O_uart_tx"]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = INIT | SEND_CONFIG | WAIT_CONFIG | READ_DATA | WAIT_DATA
    [@@deriving sexp_of, compare, enumerate]
  end

  module MyI2c_master = I2c_master.Make(X)
  module MyLeds = Leds.Make(X)

  (* Create GOWIN primitive components with port naming to satisfy validator *)
  let iobuf ~din ~oen =
    let m = Instantiation.create
      ~name:"IOBUF"
      ~inputs:[ "I", din; "OEN", oen ]
      ~outputs:[ "IO", 1; "O", 1 ]
      ()
    in 
    (Map.find_exn m "IO" -- "sda_io", 
     Map.find_exn m "O"  -- "sda_from_bus")
	  
  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let open Always in
    let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in

    let sm = State_machine.create (module States) sync_spec ~enable:vdd in

    (* Control Registers *)
    let start    = Variable.reg ~enable:vdd sync_spec ~width:1 in
    let reg_addr = Variable.reg ~enable:vdd sync_spec ~width:8 in
    let wdata    = Variable.reg ~enable:vdd sync_spec ~width:8 in
    let reg_rw   = Variable.reg ~enable:vdd sync_spec ~width:1 in
	  let data_out = Variable.reg ~enable:vdd sync_spec ~width:48 in
    (* Feedback Wires to handle recursive module dependencies *)
    let master_ready_wire = Signal.wire 1 in
    let sda_in_wire       = Signal.wire 1 in

    (* Instantiate the I2C Master *)
    let i2c_master = MyI2c_master.hierarchical scope (
	     MyI2c_master.I.{ reset    = input.reset
                      ; clock    = input.clock
                      ; dev_addr = of_int ~width:7 X.i2c_address
                      ; reg_addr = reg_addr.value
                      ; mosi     = wdata.value
                      ; rw       = reg_rw.value 
                      ; start    = start.value
                      ; sda_in   = sda_in_wire }) in

    let leds = MyLeds.hierarchical scope (
	     MyLeds.I.{ reset = input.reset; clock = input.clock }) in

    (* Instantiate the IOBUF for SDA *)
    let (sda_io, sda_phys_in) = iobuf 
        ~din:i2c_master.sda_out 
        ~oen:i2c_master.sda_oe in

    (* Connect wires after instantiations to complete the logic loops *)
    let () =
      master_ready_wire <== i2c_master.ready;
      sda_in_wire       <== sda_phys_in 
    in

    (* Compile the logic - Must be done before returning the record O.t *)
    let () = compile [
      sm.switch [
        States.INIT, [
          start    <-- gnd;
          sm.set_next SEND_CONFIG;
        ];
        States.SEND_CONFIG, [
          reg_addr <--. 0xF4; 
          wdata    <--. 0x27;
		  reg_rw   <-- gnd; (* Write *)
          start    <-- vdd;
          sm.set_next WAIT_CONFIG;
        ];
        States.WAIT_CONFIG, [
		  start <-- gnd;
          if_ master_ready_wire [
            sm.set_next States.READ_DATA;
          ][]
        ];
        States.READ_DATA, [
          reg_addr <--. 0xF7;
		  reg_rw   <-- vdd; (* Read *)
		  start    <-- vdd;
		  sm.set_next WAIT_DATA;
        ];
        States.WAIT_DATA, [
		  start <-- gnd;
		  if_ master_ready_wire [
		    data_out <-- i2c_master.miso;
            sm.set_next States.READ_DATA;
          ][]
        ];
      ]
    ] in

    (* Return circuit output value *)
    { O.leds    = (~:(leds.leds))
    ; O.scl     = i2c_master.scl
    ; O.sda_out = sda_io
    ; O.uart_tx = gnd
    } 

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
    
end
