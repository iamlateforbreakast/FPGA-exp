module i2c_master (
    input clk, rst_n,
    input [6:0] addr, [7:0] reg_addr, [7:0] din,
    input rw, start,
    output reg [7:0] dout, output reg done, ack_error,
    inout sda, output scl
);
    localparam DIV = 270; // 27MHz / (270) ≈ 100kHz
    reg [8:0] count;
    reg scl_clk;
    reg [4:0] state;
    reg [3:0] bit_ptr;
    reg [7:0] shift_reg;
    reg sda_out, sda_oe;

    assign scl = scl_clk;
    assign sda = sda_oe ? sda_out : 1'bz;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state <= 0; done <= 0; scl_clk <= 1; sda_oe <= 0;
        end else begin
            case (state)
                0: if (start) begin // START Condition
                    state <= 1; sda_oe <= 1; sda_out <= 0; bit_ptr <= 7;
                    shift_reg <= {addr, rw}; 
                end
                1: begin // Send Device Address + RW
                    scl_clk <= ~scl_clk;
                    if (!scl_clk) begin
                        sda_out <= shift_reg[bit_ptr];
                        if (bit_ptr == 0) state <= 2; else bit_ptr <= bit_ptr - 1;
                    end
                end
                2: begin // Wait for ACK and STOP
                    scl_clk <= ~scl_clk;
                    if (scl_clk) begin sda_oe <= 0; state <= 3; end
                end
                3: begin // Transaction Finished
                    sda_oe <= 1; sda_out <= 1; done <= 1; state <= 0;
                end
                // Additional states for Data Phase and Repeated Start required for full read
            endcase
        end
    end
endmodule
