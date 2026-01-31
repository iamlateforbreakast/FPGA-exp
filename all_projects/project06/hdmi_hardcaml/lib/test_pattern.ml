open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct
  module I = struct
    type 'a t = {
      pxl_clk    : 'a;
      rst_n      : 'a;
      mode       : 'a; [@bits 3]
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      de     : 'a;
      hs     : 'a;
      vs     : 'a;
      data_r : 'a; [@bits 8]
      data_g : 'a; [@bits 8]
      data_b : 'a; [@bits 8]
    } [@@deriving sexp_of, hardcaml]
  end

  let create (i : Signal.t I.t) =
    let spec = Reg_spec.create ~clock:i.pxl_clk ~clear:i.rst_n () in

    (* --- Counters --- *)
    let h_cnt = reg_fb ~width:12 ~f:(fun current ->
      mux2 (current >=: X.h_total -:. 1) (zero 12) (current +:. 1)) spec
    in
    
    let v_cnt = reg_fb ~width:12 ~f:(fun current ->
      let last_pixel = h_cnt >=: X.h_total -:. 1 in
      let last_line = current >=: X.v_total -:. 1 in
      mux2 last_pixel (mux2 last_line (zero 12) (current +:. 1)) current) spec
    in

    (* --- Sync Signal Generation --- *)
    let de_w = 
      ((h_cnt >=: (X.h_sync +: X.h_bporch)) &: (h_cnt <=: (X.h_sync +: X.h_bporch +: X.h_res -:. 1))) &:
      ((v_cnt >=: (X.v_sync +: X.v_bporch)) &: (v_cnt <=: (X.v_sync +: X.v_bporch +: X.v_res -:. 1)))
    in
    let hs_w = ~: ((h_cnt >=: zero 12) &: (h_cnt <=: X.h_sync -:. 1)) in
    let vs_w = ~: ((v_cnt >=: zero 12) &: (v_cnt <=: X.v_sync -:. 1)) in

    (* Pipeline Delays (N=5) *)
    let de_dn = pipeline spec ~n:5 de_w in
    let hs_dn = pipeline spec ~n:5 hs_w in
    let vs_dn = pipeline spec ~n:5 vs_w in

    (* --- Display Area Counters --- *)
    let de_pos = (~~ (msb (pipeline spec ~n:1 de_w))) &: de_w in (* Simplified edge detect *)
    
    let de_hcnt = always_fb ~width:12 ~f:(fun curr ->
      mux2 de_pos (zero 12) (mux2 (pipeline spec ~n:1 de_w) (curr +:. 1) curr)) spec
    in

    (* --- Pattern Logic (Example: Color Bar) --- *)
    let color_trig_num = always_fb ~width:12 ~f:(fun curr ->
      let de_active = pipeline spec ~n:1 de_w in
      mux2 (~~ de_active) (select X.h_res 11 3 @: zero 3) curr (* Simplified scaling *)) spec
    in

    (* --- Output Assignment --- *)
    { O.
      de = de_dn;
      hs = mux2 X.hs_pol (~~ hs_dn) hs_dn;
      vs = mux2 X.vs_pol (~~ vs_dn) vs_dn;
      data_r = X.single_r; (* Logic for mode selection omitted for brevity *)
      data_g = X.single_g;
      data_b = X.single_b;
    }
end

