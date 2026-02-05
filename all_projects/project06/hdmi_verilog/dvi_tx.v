module DVI_TX_Top(
  I_rst_n,
  I_serial_clk,
  I_rgb_clk,
  I_rgb_vs,
  I_rgb_hs,
  I_rgb_de,
  I_rgb_r,
  I_rgb_g,
  I_rgb_b,
  O_tmds_clk_p,
  O_tmds_clk_n,
  O_tmds_data_p,
  O_tmds_data_n
);

input I_rst_n;
input I_serial_clk;
input I_rgb_clk;
input I_rgb_vs;
input I_rgb_hs;
input I_rgb_de;
input [7:0] I_rgb_r;
input [7:0] I_rgb_g;
input [7:0] I_rgb_b;
output O_tmds_clk_p;
output O_tmds_clk_n;
output [2:0] O_tmds_data_p;
output [2:0] O_tmds_data_n;

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


TLVDS_OBUF clk_obuf(.O(O_tmds_clk_p), .OB(O_tmds_clk_n), .I(serialized_clock));
TLVDS_OBUF blu_obuf(.O(O_tmds_data_p[0]), .OB(O_tmds_data_n[0]), .I(serialized_blue));
TLVDS_OBUF grn_obuf(.O(O_tmds_data_p[1]), .OB(O_tmds_data_n[1]), .I(serialized_green));
TLVDS_OBUF red_obuf(.O(O_tmds_data_p[2]), .OB(O_tmds_data_n[2]), .I(serialized_red));

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

endmodule
