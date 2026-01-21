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

  let display_rom ~clock ~read_address =
    let open Signal in
    (* 1. Prepare initial data (128 * 8 elements) *)
    let initial_data = Array.init (128 * 8) (fun i -> 
      of_int ~width:8 (i mod 256)) in

    (* 2. Define the Read Port *)
    let read_port = 
      { read_clock = clock
      ; read_address = read_address
      ; read_enable = vdd
      } 
    in

    (* 3. Create the memory. For a ROM, write_ports is an empty array. *)
    (* initialize_with is often a parameter or a specialized constructor like Ram.create *)
    let outputs = multiport_memory 1024
      ~write_ports:[||]
      ~read_ports:[| read_port |]
    in
    outputs.(0) (* Return data from the first (only) read port *)
  
  let create (_scope: Scope.t) (i: _ I.t) : _ O.t =
    let open Always in
    let open Signal in
    (* let { I.clock; i_reset } = i in *)
    (* Create synchronous registers *)
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:i.i_reset () in

    (* State machine definition *)    
    let sm = Always.State_machine.create (module States) reg_sync_spec ~enable:vdd in

    (* Instantiate the synchronous RAM *)
    let ram_out = display_rom ~clock:i.clock ~read_address:column_index.value in
  
    (* Registers *)
    
    (* The program block with a call to [compile]. *)
    
    { O.o_sclk = sclk.value
    ; O.o_sdin = sdin.value
    ; O.o_cs = ~:(ncs.value)
    ; O.o_dc = ~:(ndc.value)
    ; O.o_reset = ~:(nreset.value)
    }

end
