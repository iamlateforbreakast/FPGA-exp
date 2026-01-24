module bmp280_ctrl (
    input clk, rst_n,
    output [19:0] raw_temp, raw_press,
    inout sda, output scl
);
    reg [3:0] state;
    reg start;
    wire done;
    reg [7:0] reg_addr, wdata;
    
    i2c_master master (
        .clk(clk), .rst_n(rst_n), .addr(7'h76), // Default 0x76
        .reg_addr(reg_addr), .din(wdata), .start(start),
        .done(done), .sda(sda), .scl(scl)
    );

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) state <= 0;
        else case (state)
            0: begin // Step 1: Configure Sensor
                reg_addr <= 8'hF4; wdata <= 8'h27; start <= 1;
                if (done) state <= 1;
            end
            1: begin // Step 2: Request Pressure MSB (0xF7)
                reg_addr <= 8'hF7; start <= 1;
                if (done) state <= 2;
            end
            // Continue sequencing through all 6 data registers
        endcase
    end
endmodule
