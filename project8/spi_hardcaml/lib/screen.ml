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

  let create (_scope: Scope.t) (_i: _ I.t) =
    let open Signal in
    {O.io_sclk = gnd; O.io_sdin = gnd; io_cs = gnd; io_dc = gnd; io_reset = gnd}

end
