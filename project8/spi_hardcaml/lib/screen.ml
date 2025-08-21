(* screen.ml *)
open Hardcaml

module type Config = Config_intf.S

module Make (X : Config) = struct

  let startup_wait_parameter = Parameter.create ~name:"STARTUP_WAIT" ~value:(Parameter.Value.Int 10_000_000)

  module I = struct
    type 'a t =
      { clk : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { io_sclk : 'a
      ; io_sdin : 'a
      ; io_cs : 'a
      ; io_dc : 'a
      ; io_reset : 'a
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init_power | Send_command | Load_data
    [@@deriving sexp_of, compare, enumerate]
  end

  (*
  reg [32:0] counter = 0;
  
  
  reg dc = 1;
  reg sclk = 1;
  reg sdin = 0;
  reg reset = 1;
  reg cs = 0;
  
  reg [7:0] dataToSend = 0;
  reg [3:0] bitNumber = 0;  
  reg [9:0] pixelCounter = 0;
  *)
  
  (* State machine definition *)
  let sm = Always.State_machine.create (module States) spec ~enable:vdd
  
  
  let create (_scope: Scope.t) (_i: _ I.t) =
    let open Signal in
    {O.io_sclk = gnd; O.io_sdin = gnd; io_cs = gnd; io_dc = gnd; io_reset = gnd}

  always @(posedge clk) begin
    case (state)
      state_init_power: begin
        if (counter == STARTUP_WAIT) begin
          state <= state_send_command;
          counter <= 0;
        end else begin
          counter <= counter + 1;
        end
      end
      state_send_command: begin
        // Some logic to load the command
        state <= state_load_data;
      end
      state_load_data: begin
        // Some logic to load pixel data
        state <= state_send_command;
      end
      default: begin
        state <= state_init_power;
      end
    endcase
  end

end
