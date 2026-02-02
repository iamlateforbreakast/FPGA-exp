(* dvi_encoder.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t = {
      rst_n   : 'a;
      pix_clk : 'a;
      de      : 'a;
      control : 'a; [@bits 2]
      data    : 'a; [@bits 8]
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      encoded : 'a; [@bits 10]
    } [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) (input : _ I.t) =
    let spec = Reg_spec.create ~clock:input.pix_clk ~reset:input.rst_n () in

    (* Stage 1: Minimize transitions *)
    (* Using of_int for the width-specific constants *)
    let qm_0 = 
      let res = Array.make 9 (of_int ~width:1 1) in
      res.(0) <- select input.data 0 0;
      for i = 1 to 7 do
        res.(i) <- res.(i-1) ^: (select input.data i i)
      done;
      concat_msb (Array.to_list res |> List.rev)
    in

    let qm_1 = 
      let res = Array.make 9 (of_int ~width:1 0) in
      res.(0) <- select input.data 0 0;
      for i = 1 to 7 do
        res.(i) <- res.(i-1) ^: ~:(select input.data i i)
      done;
      concat_msb (Array.to_list res |> List.rev)
    in

    let ones_in_data = popcount input.data in
    let qm = mux2 ((ones_in_data >: of_int ~width:4 4) |: 
                  ((ones_in_data ==: of_int ~width:4 4) &: ~:(select input.data 0 0))) 
                  qm_1 qm_0 in

    let qm_7_0 = select qm 7 0 in
    let qm_8 = select qm 8 8 in
    let ones_in_qm = popcount qm_7_0 in
    (* let disparity = (ones_in_qm <<: 1) -: of_int ~width:5 8 in *)
    let disparity = (concat_lsb [ones_in_qm; gnd]) -: (of_int ~width:5 8) in

    (* let bias = Always.Variable.reg spec ~width:5 () in *)
    let bias = Always.Variable.reg ~enable:vdd (Reg_spec.create ~clock:input.pix_clk ~clear:~:(input.rst_n) ()) ~width:5 in
    (* let encoded = Always.Variable.reg spec ~width:10 () in *)
    let encoded = Always.Variable.reg ~enable:vdd (Reg_spec.create ~clock:input.pix_clk ~clear:~:(input.rst_n) ()) ~width:10 in
    let invert = (select bias.value 4 4) ^: (ones_in_qm >: of_int ~width:4 4) in

    let open Always in
    compile [
      if_ ~:(input.de) [
          bias <--. 0;
          switch input.control [
            of_int ~width:2 0, [encoded <-- of_string "1101010100"];
            of_int ~width:2 1, [encoded <-- of_string "0010101011"];
            of_int ~width:2 2, [encoded <-- of_string "0101010100"];
            of_int ~width:2 3, [encoded <-- of_string "1010101011"];
          ];
          ] [
        if_ ((bias.value ==: (of_int ~width:5 0)) |: (ones_in_qm ==: of_int ~width:4 4)) [
          encoded <-- concat_msb [ ~:qm_8; qm_8; (mux2 qm_8 qm_7_0 (~:qm_7_0)) ];
          bias <-- mux2 qm_8 (bias.value +: disparity) (bias.value -: disparity);
        ] [
          if_ (invert) [
            encoded <-- concat_msb [ of_int ~width:1 1; qm_8; qm_7_0 ^: (repeat (of_int ~width:1 1) 8) ];
            bias <-- bias.value +: (concat_msb [qm_8; of_int ~width:1 0]) -: disparity;
          ] [
            encoded <-- concat_msb [ of_int ~width:1 0; qm_8; qm_7_0 ];
            bias <-- bias.value -: (concat_msb [~:qm_8; of_int ~width:1 0]) +: disparity;
          ]
        ]
      ]
    ];

    { O.encoded = encoded.value }

end
