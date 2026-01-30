open Hardcaml
open Signal

(**
 * This Hardcaml module is a direct translation of the Verilog `TMDS_rPLL` module,
 * which instantiates a Gowin `rPLL` primitive.
 *)

(* Define the input interface, corresponding to the Verilog module's inputs. *)
module I = struct
  type 'a t = { clkin : 'a [@rtlname "clkin"] (* input clkin; *) }
  [@@deriving hardcaml]
end

(* Define the output interface, corresponding to the Verilog module's outputs. *)
module O = struct
  type 'a t = {
    clkout : 'a [@rtlname "clkout"]; (* output clkout; *)
    lock : 'a [@rtlname "lock"]; (* output lock; *)
  }
  [@@deriving hardcaml]
end

(**
 * The main function to create the `TMDS_rPLL` circuit logic.
 * @param _scope The circuit scope (unused in this specific design).
 * @param i The input interface signals.
 * @return The output interface signals.
 *)
let create (_scope : Scope.t) (i : t I.t) : t O.t =
  (* These parameters correspond to the `defparam` statements in the Verilog code.
     They configure the behavior of the `rPLL` primitive. *)
  let parameters =
    [
      Parameter.string "FCLKIN" "27";
      Parameter.string "DYN_IDIV_SEL" "false";
      Parameter.int "IDIV_SEL" 3;
      Parameter.string "DYN_FBDIV_SEL" "false";
      Parameter.int "FBDIV_SEL" 54;
      Parameter.string "DYN_ODIV_SEL" "false";
      Parameter.int "ODIV_SEL" 2;
      Parameter.string "PSDA_SEL" "0000";
      Parameter.string "DYN_DA_EN" "true";
      Parameter.string "DUTYDA_SEL" "1000";
      Parameter.int "CLKOUT_FT_DIR" 1;
      Parameter.int "CLKOUTP_FT_DIR" 1;
      Parameter.int "CLKOUT_DLY_STEP" 0;
      Parameter.int "CLKOUTP_DLY_STEP" 0;
      Parameter.string "CLKFB_SEL" "internal";
      Parameter.string "CLKOUT_BYPASS" "false";
      Parameter.string "CLKOUTP_BYPASS" "false";
      Parameter.string "CLKOUTD_BYPASS" "false";
      Parameter.int "DYN_SDIV_SEL" 2;
      Parameter.string "CLKOUTD_SRC" "CLKOUT";
      Parameter.string "CLKOUTD3_SRC" "CLKOUT";
      Parameter.string "DEVICE" "GW2AR-18C";
    ]
  in

  (* Create wires for the outputs of the rPLL primitive.
     The instantiation will drive these wires. *)
  let clkout = wire 1 in
  let lock = wire 1 in

  (* Internal wires for rPLL outputs that are not top-level ports. *)
  let clkoutp_o = wire 1 in
  let clkoutd_o = wire 1 in
  let clkoutd3_o = wire 1 in

  (* Instantiate the `rPLL` primitive.
     This is the Hardcaml equivalent of the `rPLL rpll_inst (...)` block. *)
  let _ =
    Instantiation.create
      ~name:"rPLL" (* The name of the primitive module to instantiate *)
      ~instance:"rpll_inst" (* The instance name, same as in Verilog *)
      ~parameters
      ~inputs:
        [
          (* Connect the top-level input `clkin` *)
          ("CLKIN", i.clkin);
          (* The remaining inputs are tied to ground (1'b0), represented
             by `gnd` for single bits or `zero <width>` for buses. *)
          ("RESET", gnd);
          ("RESET_P", gnd);
          ("CLKFB", gnd);
          ("FBDSEL", zero 6);
          ("IDSEL", zero 6);
          ("ODSEL", zero 6);
          ("PSDA", zero 4);
          ("DUTYDA", zero 4);
          ("FDLY", zero 4);
        ]
      ~outputs:
        [
          (* Connect the output wires that will be routed to top-level ports *)
          ("CLKOUT", clkout);
          ("LOCK", lock);
          (* Connect the unused internal output wires *)
          ("CLKOUTP", clkoutp_o);
          ("CLKOUTD", clkoutd_o);
          ("CLKOUTD3", clkoutd3_o);
        ]
      ()
  in

  (* Return the populated output interface record. *)
  { O.clkout; lock }

(* Define a reusable circuit with the specified interfaces and creation logic. *)
let circuit =
  let module C = Circuit.With_interface (I) (O) in
  C.create_with_scope ~name:"TMDS_rPLL" create

(* To use this code, you would typically compile it and then generate the Verilog.
   For example, you could add the following to print the result to the console:

   let () = Rtl.print Verilog circuit
*)
