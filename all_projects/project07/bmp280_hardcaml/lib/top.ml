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
    type t = INIT | SEND_CONFIG | WAIT_CONFIG | READ_DATA | WAIT_DATA | PRINT_DATA
    [@@deriving sexp_of, compare, enumerate]
  end

  module MyI2c_master = I2c_master.Make(X)
  module MyUart_tx = Uart_tx.Make(X)
  module MyLeds = Leds.Make(X)

  let message_rom ~index =
    let open Signal in
    let chars = X.message |> String.to_list in
    let rom = List.map ~f:(fun c -> of_char c) chars in
    mux index rom

  (* let print_message ~index =
    let open Signal in
    let label = "Temp: XXXX Press: XXXX" in
    let chars = label |> String.to_seq |> List.of_seq in
    let rom = List.map ~f:(fun c -> of_char c) chars in
    let result =
      if (index.value < (of_int ~width:8 5)) then (mux index rom) else
      if (index.value < (of_int ~width:8 9)) then (of_int ~width:8 32) else
      if (index.value < (of_int ~width:8 17)) then mux index rom else
      if (index.value < (of_int ~width:8 21)) then (of_int ~width:8 32) in
    result *)
	
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
    let tx_cnt = Variable.reg ~enable:vdd sync_spec ~width:8 in
    let tx_data_valid = Variable.reg ~enable:vdd sync_spec ~width:1 in
    (* Feedback Wires to handle recursive module dependencies *)
    let master_ready_wire = Signal.wire 1 in
    let sda_in_wire       = Signal.wire 1 in

    let tx_data = Always.Variable.wire ~default:(Signal.zero 8) in

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

    (* Instanciate UART TX *)
    let uart_tx = MyUart_tx.hierarchical scope (
      MyUart_tx.I.{ clock = input.clock
                  ; reset = input.reset
                  ; data = tx_data.value
                  ; data_valid = tx_data_valid.value
                  } 
    )
    in
    (* Instantiate the IOBUF for SDA *)
    let (sda_io, sda_phys_in) = iobuf 
        ~din:i2c_master.sda_out 
        ~oen:i2c_master.sda_oe in

    (* Output data latch *)
	let raw_data_latch = Signal.reg sync_spec ~enable:i2c_master.ready i2c_master.miso in

	(* BMP280 Data Mapping (48 bits total)
       [47:40] Press MSB | [39:32] Press LSB | [31:24] Press XLSB
       [23:16] Temp MSB  | [15:8]  Temp LSB  | [7:0]   Temp XLSB *)
    let _adc_p = (select raw_data_latch 47 28) in (* 20-bit Pressure *)
    let _adc_t = (select raw_data_latch 23 4)  in (* 20-bit Temperature *)
	
    (* Connect wires after instantiations to complete the logic loops *)
    let () =
      master_ready_wire <== i2c_master.ready;
      sda_in_wire       <== sda_phys_in;
    in

    let tx_data_ready = uart_tx.data_ready in

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
            sm.set_next States.PRINT_DATA; (* Proceed to UART print *)
          ][]
        ];
        States.PRINT_DATA, [
          tx_data <-- message_rom ~index:tx_cnt.value;
          if_ (tx_cnt.value ==:. String.length(X.message)) [
            tx_cnt <-- (Signal.zero 8);
            sm.set_next States.READ_DATA; (* Loop back to read data again *)
          ] [
            if_ (tx_data_ready) [
              tx_data_valid <-- vdd;
              tx_cnt <-- tx_cnt.value +:. 1;
            ] [
              tx_data_valid <-- gnd;
            ];
          ];

        ];
      ]
    ] in

    (* Return circuit output value *)
    { O.leds    = (~:(leds.leds))
    ; O.scl     = i2c_master.scl
    ; O.sda_out = sda_io
    ; O.uart_tx = uart_tx.pin
    } 

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
    
end
