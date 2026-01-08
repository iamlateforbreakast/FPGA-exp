(* uart.ml *)

module Make (Config : Config.S) = struct
  open Config

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result_point : 'a Mixed_add.Xyzt.t [@rtl_prefix "o_"]
      ; result_point_valid : 'a
      ; last_result_point : 'a
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  let create
    ~build_mode
    scope
    { I.clock; clear }
    =
    let ( -- ) = Scope.naming scope in
    let open Always in
    (* preprocess the scalar *)
    let spec = Reg_spec.create ~clock () in
    let spec_with_clear = Reg_spec.create ~clear ~clock () in
    let sm = State_machine.create (module State) spec_with_clear in
    
    let tx =
      Uart_tx.hierarchical
        ~build_mode
        scope
        { Uart_tx.I.clock
        ; clear
        }
    ;;
    
  let hierarchical ?instance ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"top" ~scope (create ~build_mode)
  ;;
end
