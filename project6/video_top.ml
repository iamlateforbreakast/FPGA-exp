open Hardcaml

module Video_top = struct
  open Signal

  let video_top
      ~i_clk
      ~i_rst_n
    =
    (* Outputs *)
    let o_tmds_clk_p    = output "O_tmds_clk_p" 1 in
    let o_tmds_clk_n    = output "O_tmds_clk_n" 1 in
    let o_tmds_data_p   = output "O_tmds_data_p" 3 in
    let o_tmds_data_n   = output "O_tmds_data_n" 3 in

    (* Internal signals *)
    let run_cnt = reg "run_cnt" ~width:32 ~clock:i_clk ~reset:i_rst_n in
    let running = wire "running" 1 in

    let tp0_vs_in  = wire "tp0_vs_in" 1 in
    let tp0_hs_in  = wire "tp0_hs_in" 1 in
    let tp0_de_in  = wire "tp0_de_in" 1 in
    let tp0_data_r = wire "tp0_data_r" 8 in
    let tp0_data_g = wire "tp0_data_g" 8 in
    let tp0_data_b = wire "tp0_data_b" 8 in

    let vs_r   = reg "vs_r" ~width:1 ~clock:i_clk ~reset:i_rst_n in
    let cnt_vs = reg "cnt_vs" ~width:10 ~clock:i_clk ~reset:i_rst_n in

    let serial_clk = wire "serial_clk" 1 in
    let pll_lock   = wire "pll_lock" 1 in
    let hdmi4_rst_n = wire "hdmi4_rst_n" 1 in
    let pix_clk    = wire "pix_clk" 1 in

    (* LED test counter *)
    always [
      if_ (not i_rst_n) [
        run_cnt <==. zero 32
      ] ~else_:[
        if_ (run_cnt ==:. 27_000_000) [
          run_cnt <==. zero 32
        ] ~else_:[
          run_cnt <==. run_cnt +:. 1
        ]
      ]
    ];

    running <==. (run_cnt <:. 14_000_000);

    (* Testpattern instantiation (stub, needs implementation) *)
    (* Connect signals as per Verilog *)

    (* VS sync logic *)
    always [
      vs_r <==. tp0_vs_in
    ] ~clock:pix_clk;

    always [
      if_ (not hdmi4_rst_n) [
        cnt_vs <==. zero 10
      ] ~else_:[
        if_ (vs_r &. (not tp0_vs_in)) [
          cnt_vs <==. cnt_vs +:. 1
        ]
      ]
    ] ~clock:pix_clk;

    (* TMDS PLL instantiation (stub) *)
    (* serial_clk, pll_lock outputs *)

    hdmi4_rst_n <==. (i_rst_n &. pll_lock);

    (* CLKDIV instantiation (stub) *)
    (* pix_clk output *)

    (* DVI_TX_Top instantiation (stub) *)
    (* Connect all signals as per Verilog *)

    (* Return outputs *)
    [
      o_tmds_clk_p;
      o_tmds_clk_n;
      o_tmds_data_p;
      o_tmds_data_n;
    ]
end
