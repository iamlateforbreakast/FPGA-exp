(* dvi_encoder.ml *)
open Hardcaml
  
module I = struct
  type 'a t =
    { rst_n : 'a
    ; pix_clk : 'a
    ; de : 'a
    ; control : 'a [@bits 2]
    ; data : 'a [@bits 8]
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { encoded : 'a [@bits 10];
    }
  [@@deriving hardcaml]
end

let create (_scope : Scope.t) (i : _ I.t) =
  let open Signal in
  (* 'ones' function translated *)
  let ones_in_data = popcount i.data in
  let qm_0 =
      (one 1) @: 
      ((i.data.:(0)) ^: (i.data.:(1)) ^: (i.data.:(2)) ^: (i.data.:(3)) ^: (i.data.:(4)) ^: (i.data.:(5)) ^: (i.data.:(6)) ^: (i.data.:(7))) @:
      ((i.data.:(0)) ^: (i.data.:(1)) ^: (i.data.:(2)) ^: (i.data.:(3)) ^: (i.data.:(4)) ^: (i.data.:(5)) ^: (i.data.:(6))) @:
      ((i.data.:(0)) ^: (i.data.:(1)) ^: (i.data.:(2)) ^: (i.data.:(3)) ^: (i.data.:(4)) ^: (i.data.:(5))) @:
      ((i.data.:(0)) ^: (i.data.:(1)) ^: (i.data.:(2)) ^: (i.data.:(3)) ^: (i.data.:(4))) @:
      ((i.data.:(0)) ^: (i.data.:(1)) ^: (i.data.:(2)) ^: (i.data.:(3))) @:
      ((i.data.:(0)) ^: (i.data.:(1)) ^: (i.data.:(2))) @:
      ((i.data.:(0)) ^: (i.data.:(1))) @:
      (i.data.:(0))
    in
  let qm_1 = 
      (one 1) @: 
      ((i.data.:(0)) ^: (~:(i.data.:(1))) ^: (~:(i.data.:(2))) ^: (~:(i.data.:(3))) ^: (~:(i.data.:(4))) ^: (~:(i.data.:(5))) ^: (~:(i.data.:(6))) ^: (~:(i.data.:(7)))) @:
      ((i.data.:(0)) ^: (~:(i.data.:(1))) ^: (~:(i.data.:(2))) ^: (~:(i.data.:(3))) ^: (~:(i.data.:(4))) ^: (~:(i.data.:(5))) ^: (~:(i.data.:(6)))) @:
      ((i.data.:(0)) ^: (~:(i.data.:(1))) ^: (~:(i.data.:(2))) ^: (~:(i.data.:(3))) ^: (~:(i.data.:(4))) ^: (~:(i.data.:(5)))) @:
      ((i.data.:(0)) ^: (~:(i.data.:(1))) ^: (~:(i.data.:(2))) ^: (~:(i.data.:(3))) ^: (~:(i.data.:(4)))) @:
      ((i.data.:(0)) ^: (~:(i.data.:(1))) ^: (~:(i.data.:(2))) ^: (~:(i.data.:(3)))) @:
      ((i.data.:(0)) ^: (~:(i.data.:(1))) ^: (~:(i.data.:(2)))) @:
      ((i.data.:(0)) ^: (~:(i.data.:(1)))) @:
      (i.data.:(0))
  in
    let qm = mux2 ((ones_in_data >:. 4) |: ((ones_in_data ==:. 4) &: ~:(i.data.:(0)))) qm_1 qm_0 in
    let ones_in_qm = popcount (select qm 7 0) in
    let disparity = (concat_lsb [ones_in_qm; gnd]) -: (of_int ~width:5 8) in
    let open Always in
    let bias_reg = Variable.reg ~enable:vdd (Reg_spec.create ~clock:i.pix_clk ~clear:~:(i.rst_n) ()) ~width:5 in
    let encoded_reg = Variable.reg ~enable:vdd (Reg_spec.create ~clock:i.pix_clk ~clear:~:(i.rst_n) ()) ~width:10 in
    let invert = Variable.reg ~enable:vdd (Reg_spec.create ~clock:i.pix_clk ~clear:~:(i.rst_n) ()) ~width:1 in
      compile [
        if_ ~:(i.de) [
          bias_reg <--. 0;
          switch i.control [
            of_int ~width:2 0, [encoded_reg <-- of_string "1101010100"];
            of_int ~width:2 1, [encoded_reg <-- of_string "0010101011"];
            of_int ~width:2 2, [encoded_reg <-- of_string "0101010100"];
            of_int ~width:2 3, [encoded_reg <-- of_string "1010101011"];
          ];
          ] [
        if_ ((bias_reg.value ==:. 0) |: (ones_in_qm ==:. 4)) [
          encoded_reg <-- (mux2 qm.:(8) (one 1 @: ~:(one 1) @: (select qm 7 0)) (~:(one 1) @: one 1 @: ~:(select qm 7 0)));
          bias_reg <-- (mux2 qm.:(8) (bias_reg.value +: disparity) (bias_reg.value -: disparity));
        ] [
          invert <-- (bias_reg.value.:(4)) ^: ((select ones_in_qm 3 2) <>:. 0);
          encoded_reg <-- (invert.value @: (qm.:(8)) @: ((select qm 7 0) ^: (repeat invert.value 8)));
          bias_reg <-- (mux2 invert.value 
            (bias_reg.value +: ((zero 3) @: (qm.:(8)) @: gnd) -: disparity)
            (bias_reg.value -: ((zero 3) @: (~:qm.:(8)) @: gnd) +: disparity));
        ];
      ];
    ];
  { O.encoded = encoded_reg.value }

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"dvi_encoder" ~instance:"inst" create i