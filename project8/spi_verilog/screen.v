module screen
#(
  parameter STARTUP_WAIT = 32'd10000000
)
(
    input clk,
    output io_sclk,
    output io_sdin,
    output io_cs,
    output io_dc,
    output io_reset
);
  localparam STATE_INIT_POWER = 8'd0;
  localparam STATE_LOAD_INIT_CMD = 8'd1;
  localparam STATE_SEND = 8'd2;
  localparam STATE_CHECK_FINISHED_INIT = 8'd3;
  localparam STATE_LOAD_DATA = 8'd4;
  
  
endmodule
