open Hardcaml
open Signal
open Hardcaml.Reg

module Svo_hdmi (
  input : sig
    val clk : Clock.t
    val resetn : bool
    val clk_pixel : Clock.t
    val clk_5x_pixel : Clock.t
    val locked : bool
  end,
  output : sig
    val tmds_clk_n : bool
    val tmds_clk_p : bool
    val tmds_d_n : Bits.t<width=3>
    val tmds_d_p : Bits.t<width=3>
  end
) = struct

  (* Parameters *)
  let svo_mode = "1920x1080"
  let svo_framerate = 60
  let svo_bits_per_pixel = 24
  let svo_bits_per_red = 8
  let svo_bits_per_green = 8
  let svo_bits_per_blue = 8
  let svo_bits_per_alpha = 0

  let locked_clk_q = reg_fb ~width:4 ~clock:input.clk (fun q -> q.:(3, 0) @: input.locked)
  let resetn_clk_pixel_q = reg_fb ~width:4 ~clock:input.clk_pixel (fun q -> q.:(3, 0) @: input.resetn)

  let clk_resetn = input.resetn & (select_bit locked_clk_q 3)
  let clk_pixel_resetn = input.locked & (select_bit resetn_clk_pixel_q 3)

  let svo_tcard_inst = 
    Svo_tcard.create {
      Svo_tcard.input with
      clk = input.clk_pixel;
      resetn = input.resetn;
    }
  let vdma_tvalid = svo_tcard_inst.output.out_axis_tvalid
  let vdma_tready = wire (set_width 1)
  let vdma_tdata = svo_tcard_inst.output.out_axis_tdata
  let vdma_tuser = svo_tcard_inst.output.out_axis_tuser

  let svo_enc_inst =
    Svo_enc.create {
      Svo_enc.input with
      clk = input.clk_pixel;
      resetn = clk_pixel_resetn;
      in_axis_tvalid = vdma_tvalid;
      in_axis_tready = vdma_tready;
      in_axis_tdata = vdma_tdata;
      in_axis_tuser = vdma_tuser;
    }
  let video_enc_tvalid = svo_enc_inst.output.out_axis_tvalid
  let video_enc_tready = of_bit gnd
  let video_enc_tdata = svo_enc_inst.output.out_axis_tdata
  let video_enc_tuser = svo_enc_inst.output.out_axis_tuser

  let () = vdma_tready <-- of_bit (gnd |> of_bit)

  let svo_tmds_0_inst = 
    Svo_tmds.create {
      Svo_tmds.input with
      clk = input.clk_pixel;
      resetn = clk_pixel_resetn;
      de = ~((select_bit video_enc_tuser 3));
      ctrl = select_bits video_enc_tuser (2, 1);
      din = select_bits video_enc_tdata (23, 16);
    }
  
  let svo_tmds_1_inst = 
    Svo_tmds.create {
      Svo_tmds.input with
      clk = input.clk_pixel;
      resetn = clk_pixel_resetn;
      de = ~((select_bit video_enc_tuser 3));
      ctrl = of_bits (of_int 2 0);
      din = select_bits video_enc_tdata (15, 8);
    }
  
  let svo_tmds_2_inst = 
    Svo_tmds.create {
      Svo_tmds.input with
      clk = input.clk_pixel;
      resetn = clk_pixel_resetn;
      de = ~((select_bit video_enc_tuser 3));
      ctrl = of_bits (of_int 2 0);
      din = select_bits video_enc_tdata (7, 0);
    }
  
  let tmds_d_list = [
    select_bit (svo_tmds_0_inst.output.dout) 0;
    select_bit (svo_tmds_1_inst.output.dout) 0;
    select_bit (svo_tmds_2_inst.output.dout) 0;
  ]
  let tmds_d = concat tmds_d_list

  let tmds_d0 = 
    select_bit (svo_tmds_0_inst.output.dout) 0 
    @: (select_bit (svo_tmds_1_inst.output.dout) 0) 
    @: (select_bit (svo_tmds_2_inst.output.dout) 0)
  
  let tmds_d1 = 
    select_bit (svo_tmds_0_inst.output.dout) 1
    @: (select_bit (svo_tmds_1_inst.output.dout) 1)
    @: (select_bit (svo_tmds_2_inst.output.dout) 1)

  let tmds_d2 =
    select_bit (svo_tmds_0_inst.output.dout) 2
    @: (select_bit (svo_tmds_1_inst.output.dout) 2)
    @: (select_bit (svo_tmds_2_inst.output.dout) 2)
  
  let tmds_d3 =
    select_bit (svo_tmds_0_inst.output.dout) 3
    @: (select_bit (svo_tmds_1_inst.output.dout) 3)
    @: (select_bit (svo_tmds_2_inst.output.dout) 3)

  let tmds_d4 =
    select_bit (svo_tmds_0_inst.output.dout) 4
    @: (select_bit (svo_tmds_1_inst.output.dout) 4)
    @: (select_bit (svo_tmds_2_inst.output.dout) 4)
  
  let tmds_d5 =
    select_bit (svo_tmds_0_inst.output.dout) 5
    @: (select_bit (svo_tmds_1_inst.output.dout) 5)
    @: (select_bit (svo_tmds_2_inst.output.dout) 5)

  let tmds_d6 =
    select_bit (svo_tmds_0_inst.output.dout) 6
    @: (select_bit (svo_tmds_1_inst.output.dout) 6)
    @: (select_bit (svo_tmds_2_inst.output.dout) 6)

  let tmds_d7 =
    select_bit (svo_tmds_0_inst.output.dout) 7
    @: (select_bit (svo_tmds_1_inst.output.dout) 7)
    @: (select_bit (svo_tmds_2_inst.output.dout) 7)
  
  let tmds_d8 =
    select_bit (svo_tmds_0_inst.output.dout) 8
    @: (select_bit (svo_tmds_1_inst.output.dout) 8)
    @: (select_bit (svo_tmds_2_inst.output.dout) 8)

  let tmds_d9 =
    select_bit (svo_tmds_0_inst.output.dout) 9
    @: (select_bit (svo_tmds_1_inst.output.dout) 9)
    @: (select_bit (svo_tmds_2_inst.output.dout) 9)

  (* OSER10 *)
  let tmds_d = 
    let d0 = tmds_d0 in
    let d1 = tmds_d1 in
    let d2 = tmds_d2 in
    let d3 = tmds_d3 in
    let d4 = tmds_d4 in
    let d5 = tmds_d5 in
    let d6 = tmds_d6 in
    let d7 = tmds_d7 in
    let d8 = tmds_d8 in
    let d9 = tmds_d9 in
    let pclk = input.clk_pixel in
    let fclk = input.clk_5x_pixel in
    let reset = ~:clk_pixel_resetn in
    
    (* OSER10 is a primitive and should be called directly if available in your Hardcaml primitive library *)
    (* For now, we'll represent it as a black box and assign the outputs *)
    
    let tmds_d_output = wire (set_width 3) in
    
    let oser10_insts = List.init 3 (fun i ->
      Oser10.create {
        Oser10.input with
        d0 = select_bit d0 i;
        d1 = select_bit d1 i;
        d2 = select_bit d2 i;
        d3 = select_bit d3 i;
        d4 = select_bit d4 i;
        d5 = select_bit d5 i;
        d6 = select_bit d6 i;
        d7 = select_bit d7 i;
        d8 = select_bit d8 i;
        d9 = select_bit d9 i;
        pclk = pclk;
        fclk = fclk;
        reset = reset;
      }
    ) in
    
    tmds_d_output <-- concat (List.map (fun inst -> inst.output.q) oser10_insts);
    
    tmds_d_output

  (* ELVDS_OBUF *)
  let elvds_obuf_insts = List.init 4 (fun i ->
    let input_signal = 
      if i = 0 then input.clk_pixel
      else select_bit tmds_d (i-1)
    in
    Elvds_obuf.create {
      Elvds_obuf.input with
      i = input_signal;
    }
  ) in

  let tmds_p_signals = List.map (fun inst -> inst.output.o) elvds_obuf_insts in
  let tmds_n_signals = List.map (fun inst -> inst.output.ob) elvds_obuf_insts in

  let tmds_clk_p = List.hd tmds_p_signals in
  let tmds_d_p = concat (List.tl tmds_p_signals)

  let tmds_clk_n = List.hd tmds_n_signals in
  let tmds_d_n = concat (List.tl tmds_n_signals)

  let () = 
    output.tmds_clk_n = tmds_clk_n;
    output.tmds_clk_p = tmds_clk_p;
    output.tmds_d_n = tmds_d_n;
    output.tmds_d_p = tmds_d_p
end

