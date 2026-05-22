`timescale 1ns / 1ps

module tb_SPW_CODEC();

    // Clock and Reset
    reg clk;
    reg rst;

    // Control and Status
    reg  link_en;
    reg  link_dis;
    wire link_active;
    wire link_error;

    // Parallel Data Interface (Transmit)
    reg  tx_write;
    reg  [8:0] tx_data; // Bit 8 is usually EOP/Control flag
    wire tx_ready;

    // Parallel Data Interface (Receive)
    wire rx_read;
    wire [8:0] rx_data;
    wire rx_valid;

    // Serial Interface (Loopback)
    wire din, sin, dout, sout;

    // Instantiate the CODEC
    SPW_CODEC uut (
        .clk(clk),
        .rst(rst),
        .en(link_en),
        .dis(link_dis),
        .active(link_active),
        .err(link_error),
        
        // Transmit Parallel
        .tx_write(tx_write),
        .tx_data(tx_data),
        .tx_ready(tx_ready),
        
        // Receive Parallel
        .rx_read(rx_read),
        .rx_data(rx_data),
        .rx_valid(rx_valid),
        
        // Serial Interface (connected for loopback)
        .tx_d(dout),
        .tx_s(sout),
        .rx_d(din),
        .rx_s(sin)
    );

    // Loopback wiring
    assign din = dout;
    assign sin = sout;
    
    // Auto-read received data
    assign rx_read = rx_valid;

    // Clock Generation
    always #10 clk = ~clk; // 50 MHz

    initial begin
        // Initialize Signals
        clk = 0;
        rst = 1;
        link_en = 0;
        link_dis = 0;
        tx_write = 0;
        tx_data = 0;

        // Reset Sequence
        #100 rst = 0;
        #50  link_en = 1;

        // Wait for Link to become Active (Run State)
        wait(link_active);
        $display("Link is Active. Starting transmission...");

        // Send a Data Character (e.g., 0xA5)
        @(posedge clk);
        if (tx_ready) begin
            tx_data = 9'h0A5; 
            tx_write = 1;
        end
        @(posedge clk) tx_write = 0;

        // Send an EOP (End of Packet) - typically bit 8 high
        wait(tx_ready);
        @(posedge clk);
        tx_data = 9'h100; 
        tx_write = 1;
        @(posedge clk) tx_write = 0;

        // Monitor Reception
        fork
            begin
                wait(rx_valid && rx_data == 9'h0A5);
                $display("Success: Received Data 0xA5");
            end
            begin
                #5000; // Timeout
                if (!rx_valid) $display("Error: Timeout waiting for data");
            end
        join_any

        #1000 $finish;
    end

endmodule
