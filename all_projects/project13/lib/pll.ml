open Hardcaml
open Signal
open Base

(** Gowin PLL wrapper.  The Nano 20K (GW2A) uses rPLL, the Nano 4K
    (GW1NS-4) uses PLLVR.  Only CLKOUT and LOCK are used; the four-phase
    memory timing is generated in the fabric, so no phase-shifted PLL output
    is required. *)
module Make (X : Config_intf.S) = struct
  module I = struct
    type 'a t = { clkin : 'a } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { clkout : 'a; lock : 'a } [@@deriving hardcaml]
  end

  let zeros n = List.init n ~f:(fun _ -> gnd) |> concat_msb

  let rpll ~inst ~idiv_sel ~fbdiv_sel ~odiv_sel ~device ~clkin =
    let parameters =
      List.map
        ~f:(fun (name, value) -> Parameter.create ~name ~value)
        [ "FCLKIN", Parameter.Value.String "27"
        ; "DYN_IDIV_SEL", Parameter.Value.String "false"
        ; "IDIV_SEL", Parameter.Value.Int idiv_sel
        ; "DYN_FBDIV_SEL", Parameter.Value.String "false"
        ; "FBDIV_SEL", Parameter.Value.Int fbdiv_sel
        ; "DYN_ODIV_SEL", Parameter.Value.String "false"
        ; "ODIV_SEL", Parameter.Value.Int odiv_sel
        ; "PSDA_SEL", Parameter.Value.String "0000"
        ; "DYN_DA_EN", Parameter.Value.String "true"
        ; "DUTYDA_SEL", Parameter.Value.String "1000"
        ; "CLKOUT_FT_DIR", Parameter.Value.Int 1
        ; "CLKOUTP_FT_DIR", Parameter.Value.Int 1
        ; "CLKOUT_DLY_STEP", Parameter.Value.Int 0
        ; "CLKOUTP_DLY_STEP", Parameter.Value.Int 0
        ; "CLKFB_SEL", Parameter.Value.String "internal"
        ; "CLKOUT_BYPASS", Parameter.Value.String "false"
        ; "CLKOUTP_BYPASS", Parameter.Value.String "false"
        ; "CLKOUTD_BYPASS", Parameter.Value.String "false"
        ; "DYN_SDIV_SEL", Parameter.Value.Int 2
        ; "CLKOUTD_SRC", Parameter.Value.String "CLKOUT"
        ; "CLKOUTD3_SRC", Parameter.Value.String "CLKOUT"
        ; "DEVICE", Parameter.Value.String device
        ]
    in
    let m =
      Instantiation.create
        ~name:"rPLL"
        ~instance:inst
        ~parameters
        ~inputs:
          [ "CLKIN", clkin
          ; "CLKFB", gnd
          ; "RESET", gnd
          ; "RESET_P", gnd
          ; "FBDSEL", zeros 6
          ; "IDSEL", zeros 6
          ; "ODSEL", zeros 6
          ; "PSDA", zeros 4
          ; "DUTYDA", zeros 4
          ; "FDLY", zeros 4
          ]
        ~outputs:
          [ "CLKOUT", 1
          ; "CLKOUTP", 1
          ; "CLKOUTD", 1
          ; "CLKOUTD3", 1
          ; "LOCK", 1
          ]
        ()
    in
    (Map.find_exn m "CLKOUT", Map.find_exn m "LOCK")
  ;;

  let pllvr ~inst ~idiv_sel ~fbdiv_sel ~odiv_sel ~device ~clkin =
    let parameters =
      List.map
        ~f:(fun (name, value) -> Parameter.create ~name ~value)
        [ "FCLKIN", Parameter.Value.String "27"
        ; "DYN_IDIV_SEL", Parameter.Value.String "false"
        ; "IDIV_SEL", Parameter.Value.Int idiv_sel
        ; "DYN_FBDIV_SEL", Parameter.Value.String "false"
        ; "FBDIV_SEL", Parameter.Value.Int fbdiv_sel
        ; "DYN_ODIV_SEL", Parameter.Value.String "false"
        ; "ODIV_SEL", Parameter.Value.Int odiv_sel
        ; "PSDA_SEL", Parameter.Value.String "0000"
        ; "DYN_DA_EN", Parameter.Value.String "true"
        ; "DUTYDA_SEL", Parameter.Value.String "1000"
        ; "CLKOUT_FT_DIR", Parameter.Value.Int 1
        ; "CLKOUTP_FT_DIR", Parameter.Value.Int 1
        ; "CLKOUT_DLY_STEP", Parameter.Value.Int 0
        ; "CLKOUTP_DLY_STEP", Parameter.Value.Int 0
        ; "CLKFB_SEL", Parameter.Value.String "internal"
        ; "CLKOUT_BYPASS", Parameter.Value.String "false"
        ; "CLKOUTP_BYPASS", Parameter.Value.String "false"
        ; "CLKOUTD_BYPASS", Parameter.Value.String "false"
        ; "DYN_SDIV_SEL", Parameter.Value.Int 2
        ; "CLKOUTD_SRC", Parameter.Value.String "CLKOUT"
        ; "CLKOUTD3_SRC", Parameter.Value.String "CLKOUT"
        ; "DEVICE", Parameter.Value.String device
        ]
    in
    let m =
      Instantiation.create
        ~name:"PLLVR"
        ~instance:inst
        ~parameters
        ~inputs:
          [ "CLKIN", clkin
          ; "CLKFB", gnd
          ; "RESET", gnd
          ; "RESET_P", gnd
          ; "FBDSEL", zeros 6
          ; "IDSEL", zeros 6
          ; "ODSEL", zeros 6
          ; "PSDA", zeros 4
          ; "DUTYDA", zeros 4
          ; "FDLY", zeros 4
          ; "VREN", vdd
          ]
        ~outputs:
          [ "CLKOUT", 1
          ; "CLKOUTP", 1
          ; "CLKOUTD", 1
          ; "CLKOUTD3", 1
          ; "LOCK", 1
          ]
        ()
    in
    (Map.find_exn m "CLKOUT", Map.find_exn m "LOCK")
  ;;

  let create _scope (i : Signal.t I.t) : Signal.t O.t =
    let clkout, lock =
      match X.pll_primitive with
      | `RPLL ->
        rpll
          ~inst:"pll"
          ~idiv_sel:X.pll_idiv_sel
          ~fbdiv_sel:X.pll_fbdiv_sel
          ~odiv_sel:X.pll_odiv_sel
          ~device:X.gowin_device
          ~clkin:i.clkin
      | `PLLVR ->
        pllvr
          ~inst:"pll"
          ~idiv_sel:X.pll_idiv_sel
          ~fbdiv_sel:X.pll_fbdiv_sel
          ~odiv_sel:X.pll_odiv_sel
          ~device:X.gowin_device
          ~clkin:i.clkin
    in
    { O.clkout; lock }
  ;;
  let hierarchical scope (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pll" ~instance:"inst_pll" create i
;;

end
