(* top.ml *)
open Base
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a [@rtlname "I_clk"]
      ; rst : 'a [@rtlname "I_rst"]
      ; key : 'a [@rtlname "I_key"]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
  type 'a t =
    { tmds_clk_p  : 'a [@rtlname "O_tmds_clk_p"]
    ; tmds_clk_n  : 'a [@rtlname "O_tmds_clk_n"]
    ; tmds_data_p : 'a [@rtlname "O_tmds_data_p"] [@bits 3]
    ; tmds_data_n : 'a [@rtlname "O_tmds_data_n"] [@bits 3]
    ; led         : 'a [@rtlname "O_led"] [@bits X.led_width]
    }
  [@@deriving hardcaml]
end


  module MyPattern = Test_pattern.Make(X)
  module MyKey = Key_ctrl.Make(X)
  module MyDvi_tx = Dvi_tx.Make(X)
  module MyDvi_encoder = Dvi_encoder.Make(X)
  module MyLeds = Leds.Make(X)

  let clkdiv ~div_mode ~hclkin ~resetn ~calib =
    let parameters = 
      List.map
      ~f:(fun (name, value) -> Parameter.create ~name ~value)
        [
          "DIV_MODE", Parameter.Value.String div_mode;  (* Clock Divider *)
           "GSREN", Parameter.Value.String "false";  (* Global Set/Reset Enable *)
        ] in
      Instantiation.create
        ()
        ~name:"CLKDIV" (* Must match the Gowin primitive name *)
        (* Explicit instance name so the board constraint files can pin this
           CLKDIV to a particular HCLK section with INS_LOC (see
           tangnano4k.cst); without it Hardcaml emits an auto-generated name
           that an INS_LOC line could not reliably refer to. *)
        ~instance:"clkdiv" 
        ~parameters:parameters
        ~inputs:[
          "HCLKIN", hclkin;
          "RESETN", resetn;
          "CALIB",  calib;
        ]
        ~outputs:[ "CLKOUT", 1 ]
      |> fun outputs -> Map.find_exn outputs "CLKOUT"

  (* Nano 20K (GW2A family). CLKOUT = FCLKIN * (fbdiv_sel+1) / (idiv_sel+1),
     Hz; ODIV_SEL only needs to keep VCO = CLKOUT * odiv_sel within the
     family's legal PLL range. CLKOUTD/DYN_SDIV_SEL are left wired up but
     unused (the design derives its pixel clock from a separate CLKDIV
     instance below instead, on both boards). *)
  let rpll ~inst ~idiv_sel ~fbdiv_sel ~odiv_sel ~device ~clkin =
    let parameters =
      List.map
      ~f:(fun (name, value) -> Parameter.create ~name ~value)
        [
          "FCLKIN", Parameter.Value.String "27";          (* Input clock frequency *)
		      "DYN_IDIV_SEL", Parameter.Value.String "false";
          "IDIV_SEL", Parameter.Value.Int idiv_sel;        (* Input divider *)
		      "DYN_FBDIV_SEL", Parameter.Value.String "false";
          "FBDIV_SEL", Parameter.Value.Int fbdiv_sel;      (* Feedback divider *)
		      "DYN_ODIV_SEL", Parameter.Value.String "false";
          "ODIV_SEL", Parameter.Value.Int odiv_sel;        (* Output divider for CLKOUT *)
		      "PSDA_SEL", Parameter.Value.String "0000";
		      "DYN_DA_EN", Parameter.Value.String "true";
		      "DUTYDA_SEL", Parameter.Value.String "1000";
		      "CLKOUT_FT_DIR", Parameter.Value.Int 1;
		      "CLKOUTP_FT_DIR", Parameter.Value.Int 1;
		      "CLKOUT_DLY_STEP", Parameter.Value.Int 0;
		      "CLKOUTP_DLY_STEP", Parameter.Value.Int 0;
		      "CLKFB_SEL", Parameter.Value.String "internal";
		      "CLKOUT_BYPASS", Parameter.Value.String "false";
		      "CLKOUTP_BYPASS", Parameter.Value.String "false";
		      "CLKOUTD_BYPASS", Parameter.Value.String "false";
          "DYN_SDIV_SEL", Parameter.Value.Int 2;           (* Static divider for CLKOUTD (pclk) *)
		      "CLKOUTD_SRC", Parameter.Value.String "CLKOUT";
		      "CLKOUTD3_SRC", Parameter.Value.String "CLKOUT";
          "DEVICE", Parameter.Value.String device;
        ] in
    let m = Instantiation.create
      ~name:"rPLL"
      ~instance:inst
      ~parameters:parameters
      ~inputs:[
        "CLKIN", clkin;
        "CLKFB", Signal.gnd;
        "RESET", Signal.gnd;
        "RESET_P", Signal.gnd;
      ]
      ~outputs:[
        "CLKOUT", 1;  (* Serial Clock (fclk) *)
        "CLKOUTD", 1; (* Pixel Clock (pclk) - usually fclk / 5, unused here *)
        "LOCK", 1;
      ]
      ()
    in
    (Map.find_exn m "CLKOUT", Map.find_exn m "LOCK")

  (* Nano 4K (GW1NS-4 family, chip GW1NSR-4C). This chip's PLL is exposed as
     PLLVR (voltage-regulated), not rPLL - a different primitive with a
     different port list, not just a different DEVICE parameter on rPLL.
     Parameters and port wiring verified against Sipeed's own working
     example for this exact chip
     (github.com/sipeed/TangNano-4K-example, hdmi_720p/src/gowin_pllvr/gowin_pllvr.v):
     the unused FBDSEL/IDSEL/ODSEL/PSDA/DUTYDA/FDLY buses are tied to 0 and
     VREN tied high there too. Frequency formula is the same as rPLL's:
     CLKOUT = FCLKIN * (fbdiv_sel+1) / (idiv_sel+1). *)
  let pllvr ~inst ~idiv_sel ~fbdiv_sel ~odiv_sel ~device ~clkin =
    let parameters =
      List.map
      ~f:(fun (name, value) -> Parameter.create ~name ~value)
        [
          "FCLKIN", Parameter.Value.String "27";
          "DYN_IDIV_SEL", Parameter.Value.String "false";
          "IDIV_SEL", Parameter.Value.Int idiv_sel;
          "DYN_FBDIV_SEL", Parameter.Value.String "false";
          "FBDIV_SEL", Parameter.Value.Int fbdiv_sel;
          "DYN_ODIV_SEL", Parameter.Value.String "false";
          "ODIV_SEL", Parameter.Value.Int odiv_sel;
          "PSDA_SEL", Parameter.Value.String "0000";
          "DYN_DA_EN", Parameter.Value.String "true";
          "DUTYDA_SEL", Parameter.Value.String "1000";
          "CLKOUT_FT_DIR", Parameter.Value.Int 1;
          "CLKOUTP_FT_DIR", Parameter.Value.Int 1;
          "CLKOUT_DLY_STEP", Parameter.Value.Int 0;
          "CLKOUTP_DLY_STEP", Parameter.Value.Int 0;
          "CLKFB_SEL", Parameter.Value.String "internal";
          "CLKOUT_BYPASS", Parameter.Value.String "false";
          "CLKOUTP_BYPASS", Parameter.Value.String "false";
          "CLKOUTD_BYPASS", Parameter.Value.String "false";
          "DYN_SDIV_SEL", Parameter.Value.Int 2;
          "CLKOUTD_SRC", Parameter.Value.String "CLKOUT";
          "CLKOUTD3_SRC", Parameter.Value.String "CLKOUT";
          "DEVICE", Parameter.Value.String device;
        ] in
    let zeros n = List.init n ~f:(fun _ -> Signal.gnd) |> Signal.concat_msb in
    let m = Instantiation.create
      ~name:"PLLVR"
      ~instance:inst
      ~parameters:parameters
      ~inputs:[
        "CLKIN", clkin;
        "CLKFB", Signal.gnd;
        "RESET", Signal.gnd;
        "RESET_P", Signal.gnd;
        "FBDSEL", zeros 6;
        "IDSEL", zeros 6;
        "ODSEL", zeros 6;
        "PSDA", zeros 4;
        "DUTYDA", zeros 4;
        "FDLY", zeros 4;
        "VREN", Signal.vdd;
      ]
      ~outputs:[
        "CLKOUT", 1;
        "CLKOUTP", 1;
        "CLKOUTD", 1;
        "CLKOUTD3", 1;
        "LOCK", 1;
      ]
      ()
    in
    (Map.find_exn m "CLKOUT", Map.find_exn m "LOCK")
  
  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    (* Instanciate the board's PLL primitive: rPLL on the Nano 20K (GW2A),
       PLLVR on the Nano 4K (GW1NS-4) - see the two functions above. Which
       one, and its idiv/fbdiv/odiv/device parameters, come from Config.S
       (see bin/generate.ml's Config_nano20k / Config_nano4k). *)
	  let (fclk, pll_lock) =
      match X.pll_primitive with
      | `RPLL ->
        rpll ~inst:"pll" ~idiv_sel:X.pll_idiv_sel ~fbdiv_sel:X.pll_fbdiv_sel
          ~odiv_sel:X.pll_odiv_sel ~device:X.gowin_device ~clkin:input.clock
      | `PLLVR ->
        pllvr ~inst:"pll" ~idiv_sel:X.pll_idiv_sel ~fbdiv_sel:X.pll_fbdiv_sel
          ~odiv_sel:X.pll_odiv_sel ~device:X.gowin_device ~clkin:input.clock
	  in
    (* Global reset: active low, released only when PLL is stable *)
    let global_rst_n = (~:(X.normalize_reset input.rst)) &: pll_lock in
    (* Instanciate the CLKDIV primitive *)
	  let pixel_clk = clkdiv 
        ~div_mode:"5"
        ~hclkin:fclk
        ~resetn:global_rst_n
        ~calib:gnd (* Tie CALIB to ground if unused *)
    in
  
    (* Instanciate Test Pattern*)
    let test_pattern = MyPattern.hierarchical scope (
      MyPattern.I.{ rst_n = global_rst_n
                  ; pxl_clk = pixel_clk
                  ; mode = zero 3 })
    in
    let dvi_tx = MyDvi_tx.hierarchical scope (
      MyDvi_tx.I.{ serial_clk = fclk
                  ; rst_n = global_rst_n
                  ; rgb_clk = pixel_clk
                  ; rgb_vs = test_pattern.vs
                  ; rgb_hs = test_pattern.hs
                  ; rgb_de = test_pattern.de
                  ; rgb_r = test_pattern.data_r
                  ; rgb_g = test_pattern.data_g
                  ; rgb_b = test_pattern.data_b
                  })
    in
    (* Instanciate leds *)
	  let leds = MyLeds.hierarchical scope (
	    MyLeds.I.{ reset=input.rst; clock=pixel_clk }) in

    {
      O.tmds_clk_p  = dvi_tx.tmds_clk_p;
      O.tmds_clk_n  = dvi_tx.tmds_clk_n;
      O.tmds_data_p = dvi_tx.tmds_data_p;
      O.tmds_data_n = dvi_tx.tmds_data_n;
      O.led         = X.normalize_led leds.leds;
    }


  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
end
