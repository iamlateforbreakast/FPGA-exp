(* uart.ml *)
open Hardcaml
open Signal

module Make (Config : Config.S) = struct
  (* We don't necessarily need to open Config if we use it for parameters *)
  
  module I = struct
    type 'a t =
      { clock : 'a
      ; resetn : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { tx_pin : 'a
      ; tx_data_ready : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  let create ~build_mode scope { I.clock; resetn; tx_data; tx_data_valid } =
    let ( -- ) = Scope.naming scope in

    (* Instantiate the UART Transmitter sub-module *)
    let tx =
      Uart_tx.hierarchical
        ~build_mode
        scope
        { Uart_tx.I.
          clock;
          resetn;
          tx_data;
          tx_data_valid;
        } 
    in

    (* Name the outputs for better RTL generation/debugging *)
    { O.
      tx_pin = tx.tx_pin -- "UART_TX_PIN";
      tx_data_ready = tx.tx_data_ready -- "UART_TX_READY";
    }
    
  let hierarchical ?instance ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"uart_top" ~scope (create ~build_mode)
end
