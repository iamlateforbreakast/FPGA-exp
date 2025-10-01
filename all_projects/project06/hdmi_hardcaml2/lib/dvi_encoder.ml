(* dvi_encoder.ml *)
open Hardcaml
module type Config = Config.S

module Make (X : Config) = struct
  
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
    let ones data =
      tree ~f:(+:) (List.init (width data) ~f:(fun n -> data.:(n)))
    in

    (* Combinational logic from 'assign' and 'function' statements *)
    let ones_in_data = ones i.data in

    let h_sync_time = 40 in
    let h_bporch_time = 220 in
    let h_fporch_time = 110 in
    let h_lborder_time = 0 in
    let h_rborder_time = 0 in
    let h_addr_time = 1280 in
    let v_sync_time = 5 in
    let v_bporch_time = 20 in
    let v_fporch_time = 5 in
    let v_tborder_time = 0 in
    let v_bborder_time = 0 in
    let v_addr_time = 720 in

    let h_total_time = h_sync_time + h_bporch_time + h_fporch_time + h_lborder_time + h_rborder_time + h_addr_time in
    let v_total_time = v_sync_time + v_bporch_time + v_fporch_time + v_tborder_time + v_bborder_time + v_addr_time in
    (*
    let qm_0 = 
      (one 1) @: (tree ~f:(^:) (List.init 8 ~f:(fun n -> i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 7 ~f:(fun n -> i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 6 ~f:(fun n -> i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 5 ~f:(fun n -> i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 4 ~f:(fun n -> i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 3 ~f:(fun n -> i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 2 ~f:(fun n -> i.data.:(n)))) @:
      (i.data.:(0))
    in

    let qm_1 = 
      (zero 1) @: (tree ~f:(^:) (List.init 8 ~f:(fun n -> if n=0 then i.data.:(0) else ~:i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 7 ~f:(fun n -> if n=0 then i.data.:(0) else ~:i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 6 ~f:(fun n -> if n=0 then i.data.:(0) else ~:i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 5 ~f:(fun n -> if n=0 then i.data.:(0) else ~:i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 4 ~f:(fun n -> if n=0 then i.data.:(0) else ~:i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 3 ~f:(fun n -> if n=0 then i.data.:(0) else ~:i.data.:(n)))) @:
      (tree ~f:(^:) (List.init 2 ~f:(fun n -> if n=0 then i.data.:(0) else ~:i.data.:(n)))) @:
      (i.data.:(0))
    in

    let qm = mux2 (ones_in_data >:. 4 |: (ones_in_data ==:. 4 &: ~:(i.data.:(0)))) qm_1 qm_0 in
    let ones_in_qm = ones qm.:(0,8) in
    let disparity = (concat_lsb [ones_in_qm; gnd]) -: of_int ~width:5 8 in

    let bias_reg = reg_fb (Reg_spec.create ~clock:i.pix_clk ~clear_to:(zero 5) ~clear:~:i.rst_n ()) in
    let encoded_reg = reg_fb (Reg_spec.create ~clock:i.pix_clk ~clear_to:(zero 10) ~clear:~:i.rst_n ()) in

    let open Always in
      compile [
        if_ ~:i.de [
          bias_reg <--. 0;
          switch i.control [
            of_int 0, [encoded_reg <--. Bits.of_string "1101010100"];
            of_int 1, [encoded_reg <--. Bits.of_string "0010101011"];
            of_int 2, [encoded_reg <--. Bits.of_string "0101010100"];
            of_int 3, [encoded_reg <--. Bits.of_string "1010101011"];
          ];
          ] [
        if_ ((bias_reg ==:. of_int ~width:5 0) |: (ones_in_qm ==:. of_int ~width:4 4)) [
          encoded_reg <--. (mux2 qm.:(8) (one 1 @: ~:(one 1) @: qm.:(0,8)) (~:(one 1) @: one 1 @: ~:(qm.:(0,8))));
          bias_reg <--. (mux2 qm.:(8) (bias_reg +: disparity) (bias_reg -: disparity));
        ] [
          let invert = bias_reg.:(4) ^: (ones_in_qm.:(3,2) !=:. of_int ~width:2 0) in
          encoded_reg <--. (invert @: qm.:(8) @: (qm.:(0,8) ^: (repeat invert 8)));
          bias_reg <--. (mux2 invert 
            (bias_reg +: (qm.:(8) @: gnd) -: disparity)
            (bias_reg -: (~:qm.:(8) @: gnd) +: disparity)
          );
        ];
      ];
    ];
*)
  { O.encoded = gnd }

end