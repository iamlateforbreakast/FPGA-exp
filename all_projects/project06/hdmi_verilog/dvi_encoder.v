module dvi_encoder(
  rst_n,
  pix_clk,
  de,
  data,
  control,
  encoded
);

input rst_n;
input pix_clk;
input de;
input [1:0]control;
input [7:0]data;
output reg [9:0] encoded;

reg [4:0] bias = 8'b0;

function [3:0] ones(input [7:0]data);
  ones = 4'b0 + data[0] + data[1] + data[2] + data[3] + data[4] + data[5] + data[6] + data[7];
endfunction

wire [8:0]qm_0 = {
  1'b1,
  data[0] ^ data[1] ^ data[2] ^ data[3] ^ data[4] ^ data[5] ^ data[6] ^ data[7],
  data[0] ^ data[1] ^ data[2] ^ data[3] ^ data[4] ^ data[5] ^ data[6],
  data[0] ^ data[1] ^ data[2] ^ data[3] ^ data[4] ^ data[5],
  data[0] ^ data[1] ^ data[2] ^ data[3] ^ data[4],
  data[0] ^ data[1] ^ data[2] ^ data[3],
  data[0] ^ data[1] ^ data[2],
  data[0] ^ data[1],
  data[0]
};

wire [8:0]qm_1 = {
  1'b0,
  data[0] ^ ~data[1] ^ ~data[2] ^ ~data[3] ^ ~data[4] ^ ~data[5] ^ ~data[6] ^ ~data[7],
  data[0] ^ ~data[1] ^ ~data[2] ^ ~data[3] ^ ~data[4] ^ ~data[5] ^ ~data[6],
  data[0] ^ ~data[1] ^ ~data[2] ^ ~data[3] ^ ~data[4] ^ ~data[5],
  data[0] ^ ~data[1] ^ ~data[2] ^ ~data[3] ^ ~data[4],
  data[0] ^ ~data[1] ^ ~data[2] ^ ~data[3],
  data[0] ^ ~data[1] ^ ~data[2],
  data[0] ^ ~data[1],
  data[0]
};

wire [3:0]ones_in_data = ones(data);
wire [8:0]qm = (ones_in_data > 4) || ((ones_in_data == 4) && !data[0]) ? qm_1 : qm_0;
wire [3:0]ones_in_qm = ones(qm[7:0]);
wire [4:0] disparity = {ones_in_qm, 1'b0} - 5'd8;
wire invert = bias[4] ^ (ones_in_qm[3:2] != 0);

always@(posedge pix_clk) begin
  if (!rst_n) begin
    bias <= 0;
    encoded <= 0;
  end else begin
    if (!de) begin
      bias <= 0;
      case (control)
        2'b00: encoded <= 10'b1101010100;
        2'b01: encoded <= 10'b0010101011;
        2'b10: encoded <= 10'b0101010100;
        2'b11: encoded <= 10'b1010101011;
      endcase
    end else begin
      if ((bias == 0) || (ones_in_qm == 4)) begin
        encoded[9] <= ~qm[8];
        encoded[8] <= qm[8];
        encoded[7:0] <= qm[8] ? qm[7:0] : ~qm[7:0];
        bias <= qm[8] ? bias + disparity : bias - disparity; 
      end else begin
        encoded[9] <= invert;
        encoded[8] <= qm[8];
        encoded[7:0] <= qm[7:0] ^ {8{invert}};
        bias <= invert ? bias + {qm[8], 1'b0} - disparity : bias - {~qm[8], 1'b0} + disparity;
      end
    end
  end
end

endmodule
