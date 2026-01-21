(* top.ml *)
(* screen.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
    type 'a t =
      { clock :    'a [@bits 1]  (* Need to be called clock for simulation *)
      ; i_reset :  'a [@bits 1]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_sclk :  'a [@bits 1]
      ; o_sdin :  'a [@bits 1]
      ; o_cs :    'a [@bits 1]
      ; o_dc :    'a [@bits 1]
      ; o_reset : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init_power | Load_command | Load_display | Wait_ram | Send_data | Select_data
    [@@deriving sexp_of, compare, enumerate]
  end
  
  let command_rom ~index =
    let open Signal in
    let rom = List.map (fun c -> of_int ~width:8 c) X.commands in
    mux index rom

  let display_rom (clock: Signal.t) (read_address: Signal.t) =
    let open Signal in
    (* 1. Prepare initial data (128 * 8 elements) *)
    let _initial_data = Array.init (128 * 8) (fun i -> 
      of_int ~width:8 (i mod 256)) in

    (* 3. Create the memory. For a ROM, write_ports is an empty array. *)
    (* initialize_with is often a parameter or a specialized constructor like Ram.create *)
    let outputs = Ram.create
      ~collision_mode:Write_before_read
      ~size:1024
      ~write_ports:[| |]
      ~read_ports:[| {read_clock = clock; read_address = read_address; read_enable = vdd} |]
      ()
    in
    outputs.(0) (* Return data from the first (only) read port *)
  
  module MyScreen = Screen_spi.Make(X)

  let create (scope: Scope.t) (i: _ I.t) : _ O.t =
    let open Always in
    let open Signal in
    (* let { I.clock; i_reset } = i in *)
    (* Create synchronous registers *)
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:i.i_reset () in

    (* State machine definition *)    
    let _sm = Always.State_machine.create (module States) reg_sync_spec ~enable:vdd in

    (* Registers *)
    let column_index = Variable.reg ~enable:vdd reg_sync_spec ~width:10 in

    (* Instantiate the synchronous RAM *)
    let ram_out = display_rom i.clock column_index.value in

    (* Intantiate the screen SPI controller *)
    let screen_spi = MyScreen.hierarchical scope (
      MyScreen.I.{ clock = i.clock; reset = i.i_reset; data_in = ram_out }
    ) in

    (* The program block with a call to [compile]. *)
    
    { O.o_sclk = screen_spi.sclk
    ; O.o_sdin = screen_spi.mosi
    ; O.o_cs = screen_spi.cs
    ; O.o_dc = screen_spi.dc
    ; O.o_reset = zero 1
    }

end
