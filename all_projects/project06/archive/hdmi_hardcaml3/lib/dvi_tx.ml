(* dvi_tx.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
    type 'a t =
      { i_serial_clk :    'a [@bits 1]  (* Need to be called clock for simulation *)
      ; i_resetn :  'a [@bits 1]
      ; i_rgb_clk : 'a [@bits 1]
      ; i_rgb_vs : 'a [@bits 1]
      ; i_rgb_hs : 'a [@bits 1]
      ; i_rgb_de : 'a [@bits 1]
      ; i_rgb_r : 'a [@bits 8]
      ; i_rgb_g : 'a [@bits 8]
      ; i_rgb_b : 'a [@bits 8]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_tmds_clk_p :  'a [@bits 1]
      ; o_tmds_clk_n :  'a [@bits 1]
      ; o_tmds_d_p :    'a [@bits 3]
      ; o_tmds_d_n :    'a [@bits 3]
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) =
    let open Signal in
    let encoder_blue = Dvi_encoder.hierarchical scope (
      Dvi_encoder.I.{ rst_n = i.i_resetn
                    ; pix_clk = i.i_rgb_clk
                    ; de = i.i_rgb_de
                    ; data = i.i_rgb_b
                    ; control = i.i_rgb_vs @: i.i_rgb_hs}) in
    let encoder_green = Dvi_encoder.hierarchical scope (
      Dvi_encoder.I.{ rst_n = i.i_resetn
                    ; pix_clk = i.i_rgb_clk
                    ; de = i.i_rgb_de
                    ; data = i.i_rgb_g
                    ; control = zero 2}) in
    let encoder_red = Dvi_encoder.hierarchical scope (
      Dvi_encoder.I.{ rst_n = i.i_resetn
                    ; pix_clk = i.i_rgb_clk
                    ; de = i.i_rgb_de
                    ; data = i.i_rgb_r
                    ; control = zero 2}) in
    let ser_blue = Gowin_oser10.hierarchical scope (
    Gowin_oser10.I.{ reset=(~:(i.i_resetn)); pclk=i.i_rgb_clk; fclk=i.i_serial_clk;
                      d0=encoder_blue.encoded.:(0); d1=encoder_blue.encoded.:(1);
                      d2=encoder_blue.encoded.:(2); d3=encoder_blue.encoded.:(3);
                      d4=encoder_blue.encoded.:(4); d5=encoder_blue.encoded.:(5);
                      d6=encoder_blue.encoded.:(6); d7=encoder_blue.encoded.:(7);
                      d8=encoder_blue.encoded.:(8); d9=encoder_blue.encoded.:(9) }) in
    let ser_green = Gowin_oser10.hierarchical scope (
    Gowin_oser10.I.{ reset=(~:(i.i_resetn)); pclk=i.i_rgb_clk; fclk=i.i_serial_clk;
                      d0=encoder_green.encoded.:(0); d1=encoder_green.encoded.:(1);
                      d2=encoder_green.encoded.:(2); d3=encoder_green.encoded.:(3);
                      d4=encoder_green.encoded.:(4); d5=encoder_green.encoded.:(5);
                      d6=encoder_green.encoded.:(6); d7=encoder_green.encoded.:(7);
                      d8=encoder_green.encoded.:(8); d9=encoder_green.encoded.:(9) }) in
    let ser_red = Gowin_oser10.hierarchical scope (
    Gowin_oser10.I.{ reset=(~:(i.i_resetn)); pclk=i.i_rgb_clk; fclk=i.i_serial_clk;
                      d0=encoder_red.encoded.:(0); d1=encoder_red.encoded.:(1);
                      d2=encoder_red.encoded.:(2); d3=encoder_red.encoded.:(3);
                      d4=encoder_red.encoded.:(4); d5=encoder_red.encoded.:(5);
                      d6=encoder_red.encoded.:(6); d7=encoder_red.encoded.:(7);
                      d8=encoder_red.encoded.:(8); d9=encoder_red.encoded.:(9) }) in
    let clk_ser = Gowin_oser10.hierarchical scope (
	  Gowin_oser10.I.{ reset=(~:(i.i_resetn)); pclk=i.i_rgb_clk; fclk=i.i_serial_clk;
                            d0=zero 1; d1=zero 1; d2=zero 1; d3=zero 1; d4=zero 1;
                            d5=one 1; d6=one 1; d7=one 1; d8=one 1; d9=one 1 }) in
    let clk_obuf = Gowin_tlvds_obuf.hierarchical scope (Gowin_tlvds_obuf.I.{ i=clk_ser.q }) in
    let blue_obuf = Gowin_tlvds_obuf.hierarchical scope (Gowin_tlvds_obuf.I.{ i=ser_blue.q }) in
    let green_obuf = Gowin_tlvds_obuf.hierarchical scope (Gowin_tlvds_obuf.I.{ i=ser_green.q }) in
    let red_obuf = Gowin_tlvds_obuf.hierarchical scope (Gowin_tlvds_obuf.I.{ i=ser_red.q }) in 
    { O.o_tmds_clk_n = clk_obuf.ob
    ; O.o_tmds_clk_p = clk_obuf.o
    ; O.o_tmds_d_n = red_obuf.ob @: green_obuf.ob @: blue_obuf.ob
    ; O.o_tmds_d_p = red_obuf.o @: green_obuf.o @: blue_obuf.o }
    (* The following is the Verilog code we want to translate to Hardcaml*)

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
      H.hierarchical ~scope ~name:"dvi_tx" ~instance:"inst" create i
end
(*

wire tmds_clock;
wire tmds_red;
wire tmds_green;
wire tmds_blue;


wire [9:0]encoded_blue;
wire [9:0]encoded_green;
wire [9:0]encoded_red;

wire serialized_red;
wire serialized_green;
wire serialized_blue;
wire serialized_clock;


ELVDS_OBUF clk_obuf(.O(O_tmds_clk_p), .OB(O_tmds_clk_n), .I(serialized_clock));
ELVDS_OBUF blu_obuf(.O(O_tmds_data_p[0]), .OB(O_tmds_data_n[0]), .I(serialized_blue));
ELVDS_OBUF grn_obuf(.O(O_tmds_data_p[1]), .OB(O_tmds_data_n[1]), .I(serialized_green));
ELVDS_OBUF red_obuf(.O(O_tmds_data_p[2]), .OB(O_tmds_data_n[2]), .I(serialized_red));

dvi_encoder blu_encoder(I_rst_n, I_rgb_clk, I_rgb_de, I_rgb_b, {I_rgb_vs, I_rgb_hs}, encoded_blue);
dvi_encoder grn_encoder(I_rst_n, I_rgb_clk, I_rgb_de, I_rgb_g, 2'b0, encoded_green);
dvi_encoder red_encoder(I_rst_n, I_rgb_clk, I_rgb_de, I_rgb_r, 2'b0, encoded_red);

OSER10 clk_ser( .Q(serialized_clock), 
 .D0(1'b0),
 .D1(1'b0),
 .D2(1'b0),
 .D3(1'b0),
 .D4(1'b0),
 .D5(1'b1),
 .D6(1'b1),
 .D7(1'b1),
 .D8(1'b1),
 .D9(1'b1),
 .PCLK(I_rgb_clk),
 .FCLK(I_serial_clk),
 .RESET(~I_rst_n)
);
defparam clk_ser.GSREN="false";
defparam clk_ser.LSREN ="true";

OSER10 blu_ser( .Q(serialized_blue), 
 .D0(encoded_blue[0]),
 .D1(encoded_blue[1]),
 .D2(encoded_blue[2]),
 .D3(encoded_blue[3]),
 .D4(encoded_blue[4]),
 .D5(encoded_blue[5]),
 .D6(encoded_blue[6]),
 .D7(encoded_blue[7]),
 .D8(encoded_blue[8]),
 .D9(encoded_blue[9]),
 .PCLK(I_rgb_clk),
 .FCLK(I_serial_clk),
 .RESET(~I_rst_n)
);
defparam blu_ser.GSREN="false";
defparam blu_ser.LSREN ="true";

OSER10 grn_ser( .Q(serialized_green), 
 .D0(encoded_green[0]),
 .D1(encoded_green[1]),
 .D2(encoded_green[2]),
 .D3(encoded_green[3]),
 .D4(encoded_green[4]),
 .D5(encoded_green[5]),
 .D6(encoded_green[6]),
 .D7(encoded_green[7]),
 .D8(encoded_green[8]),
 .D9(encoded_green[9]),
 .PCLK(I_rgb_clk),
 .FCLK(I_serial_clk),
 .RESET(~I_rst_n)
);
defparam grn_ser.GSREN="false";
defparam grn_ser.LSREN ="true";

OSER10 red_ser( .Q(serialized_red), 
 .D0(encoded_red[0]),
 .D1(encoded_red[1]),
 .D2(encoded_red[2]),
 .D3(encoded_red[3]),
 .D4(encoded_red[4]),
 .D5(encoded_red[5]),
 .D6(encoded_red[6]),
 .D7(encoded_red[7]),
 .D8(encoded_red[8]),
 .D9(encoded_red[9]),
 .PCLK(I_rgb_clk),
 .FCLK(I_serial_clk),
 .RESET(~I_rst_n)
);
defparam red_ser.GSREN="false";
defparam red_ser.LSREN ="true";

endmodule *)
