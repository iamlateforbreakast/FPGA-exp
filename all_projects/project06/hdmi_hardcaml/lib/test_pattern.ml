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
    let h_total = of_int ~width:12 X.h_total in
    let v_total = of_int ~width:12 X.v_total in
    let h_sync = of_int ~width:12 X.h_sync in
    let v_sync = of_int ~width:12 X.v_sync in
    let h_bporch = of_int ~width:12 X.h_bporch in
    let v_bporch = of_int ~width:12 X.v_bporch in
    let h_res = of_int ~width:12 X.h_res in
    let v_res = of_int ~width:12 X.v_res in
    let single_r = of_int ~width:8 255 in
    let single_g = of_int ~width:8 0 in
    let single_b = of_int ~width:8 0 in
    let vs_pol = of_bool X.vs_pol in
    let hs_pol = of_bool X.hs_pol in

    (* --- Counters --- *)
    let h_cnt = reg_fb spec ~width:12 ~f:(fun current ->
      mux2 (current >=: h_total -:. 1) (zero 12) (current +:. 1))
    in
    
    let v_cnt = reg_fb spec ~width:12 ~f:(fun current ->
      let last_pixel = h_cnt >=: h_total -:. 1 in
      let last_line = current >=: v_total -:. 1 in
      mux2 last_pixel (mux2 last_line (zero 12) (current +:. 1)) current)
    in

    (* --- Sync Signal Generation --- *)
    let de_w = 
      ((h_cnt >=: (h_sync +: h_bporch)) &: (h_cnt <=: (h_sync +: h_bporch +: h_res -:. 1))) &:
      ((v_cnt >=: (v_sync +: v_bporch)) &: (v_cnt <=: (v_sync +: v_bporch +: v_res -:. 1)))
    in
    let hs_w = ~: ((h_cnt >=: zero 12) &: (h_cnt <=: h_sync -:. 1)) in
    let vs_w = ~: ((v_cnt >=: zero 12) &: (v_cnt <=: v_sync -:. 1)) in

    (* Pipeline Delays (N=5) *)
    let de_dn = pipeline spec ~n:5 de_w in
    let hs_dn = pipeline spec ~n:5 hs_w in
    let vs_dn = pipeline spec ~n:5 vs_w in

    (* --- Display Area Counters --- *)
    let de_pos = (~:(msb (pipeline spec ~n:1 de_w))) &: de_w in (* Simplified edge detect *)
    
    let _de_hcnt = reg_fb spec ~width:12 ~f:(fun curr ->
      mux2 de_pos (zero 12) (mux2 (pipeline spec ~n:1 de_w) (curr +:. 1) curr))
    in

    (* --- Output Assignment --- *)
    { O.
      de = de_dn;
      hs = mux2 hs_pol (~:hs_dn) hs_dn;
      vs = mux2 vs_pol (~:vs_dn) vs_dn;
      data_r = single_r; (* Logic for mode selection omitted for brevity *)
      data_g = single_g;
      data_b = single_b;
    }
end

