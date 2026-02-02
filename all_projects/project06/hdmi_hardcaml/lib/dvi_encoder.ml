(* dvi_encoder.ml *)
open Hardcaml
open Signal

module EncodeInput = struct
  type 'a t = {
    rst_n   : 'a;
    pix_clk : 'a;
    de      : 'a;
    control : 'a; [@bits 2]
    data    : 'a; [@bits 8]
  } [@@deriving sexp_of, hardcaml]
end

module EncodeOutput = struct
  type 'a t = {
    encoded : 'a; [@bits 10]
  } [@@deriving sexp_of, hardcaml]
end

let create (scope : Scope.t) (input : _ EncodeInput.t) =
  let spec = Reg_spec.create ~clock:input.pix_clk ~reset:input.rst_n () in

  (* Helper: Count set bits *)
  let count_ones (d : Signal.t) = 
    List.init (width d) (fun i -> select d i i)
    |> List.map (reduce ~f:(+:) ) (* Convert to unsigned sum *)
    |> List.fold_left (fun acc bit -> acc +: (zero_extend bit (width acc))) (zero 4)
  in

  (* Stage 1: Minimize transitions (qm_0 is XOR, qm_1 is XNOR) *)
  let qm_0 = 
    let res = Array.make 9 (one 1) in
    res.(0) <- select input.data 0 0;
    for i = 1 to 7 do
      res.(i) <- res.(i-1) ^: (select input.data i i)
    done;
    concat_msb (Array.to_list res |> List.rev)
  in

  let qm_1 = 
    let res = Array.make 9 (zero 1) in
    res.(0) <- select input.data 0 0;
    for i = 1 to 7 do
      res.(i) <- res.(i-1) ^: ~:(select input.data i i)
    done;
    concat_msb (Array.to_list res |> List.rev)
  in

  let ones_in_data = count_ones input.data in
  let qm = mux2 ((ones_in_data >: consti 4 4) |: 
                ((ones_in_data ==: consti 4 4) &: ~:(select input.data 0 0))) 
                qm_1 qm_0 in

  (* Stage 2: DC Balancing *)
  let qm_7_0 = select qm 7 0 in
  let qm_8 = select qm 8 8 in
  let ones_in_qm = count_ones qm_7_0 in
  let disparity = (ones_in_qm <<: 1) -: consti 5 8 in

  (* State Registers *)
  let bias = Always.Variable.reg spec ~width:5 () in
  let encoded = Always.Variable.reg spec ~width:10 () in

  let invert = (select bias.value 4 4) ^: (ones_in_qm >: consti 4 4) in

  (* Logic Block *)
  let open Always in
  compile [
    if_ ~: (input.de) [
      bias <--. 0;
      encoded <-- switch input.control [
        consti 2 0, consti 10 0b1101010100;
        consti 2 1, consti 10 0b0010101011;
        consti 2 2, consti 10 0b0101010100;
        consti 2 3, consti 10 0b1010101011;
      ];
    ] [
      if_ ((bias.value ==: (zero 5)) |: (ones_in_qm ==: consti 4 4)) [
        encoded <-- concat_msb [ ~:qm_8; qm_8; (mux2 qm_8 qm_7_0 (~:qm_7_0)) ];
        bias <-- mux2 qm_8 (bias.value +: disparity) (bias.value -: disparity);
      ] [
        if_ (invert) [
          encoded <-- concat_msb [ one 1; qm_8; qm_7_0 ^: (repeat (one 1) 8) ];
          bias <-- bias.value +: (concat_msb [qm_8; zero 1]) -: disparity;
        ] [
          encoded <-- concat_msb [ zero 1; qm_8; qm_7_0 ];
          bias <-- bias.value -: (concat_msb [~:qm_8; zero 1]) +: disparity;
        ]
      ]
    ]
  ];

  { EncodeOutput.encoded = encoded.value }
