// Testbench for the generated top_level: a mode-0 SPI master at several SCLK
// rates, run with Icarus Verilog (`make sim`). It runs unchanged against both
// SPI slaves (SPI=sclk or SPI=oversampled); the oversampled one is expected
// to fail at the higher rates.
//
// Hardcaml's Cyclesim runs every register off one clock, so it cannot model a
// slave whose shift registers are clocked by SCLK while the rest of the design
// runs on the 27 MHz clock. This testbench simulates the generated Verilog
// with real, independent clocks instead.
//
// For every rate it sends random bytes and checks:
// - MISO returns the byte sent in the previous transaction (0x00 after reset),
//   sampled at each rising SCLK edge and again 2 ns later (hold);
// - o_irq is set after each byte and cleared once CS asserts again;
// - 0x41 / 0x40 turn LED bit 0 on / off.
//
// Define NANO4K for the Nano 4K build (active-low reset, one LED).
`timescale 1ns / 1ps

module tb_top;
`ifdef NANO4K
  localparam LED_W = 1;
  localparam RESET_IDLE = 1'b1;
`else
  localparam LED_W = 6;
  localparam RESET_IDLE = 1'b0;
`endif

  reg clock = 1'b0;
  reg i_reset = RESET_IDLE;
  reg i_sclk = 1'b0;
  reg i_cs_n = 1'b1;
  reg i_mosi = 1'b0;
  wire o_miso;
  wire o_irq;
  wire [LED_W-1:0] leds;

  top_level dut (
    .clock(clock), .i_reset(i_reset), .i_sclk(i_sclk), .i_cs_n(i_cs_n),
    .i_mosi(i_mosi), .o_miso(o_miso), .o_irq(o_irq), .leds(leds));

  always #18.519 clock = ~clock; // 27 MHz

  integer errors = 0;
  reg [7:0] expected = 8'h00;
  reg [7:0] rx;

  // One mode-0 transaction: MOSI and MISO change while SCLK is low and are
  // sampled on its rising edge.
  task transfer(input [7:0] tx, input real half, output [7:0] got);
    integer bit_i;
    reg sampled;
    begin
      i_cs_n = 1'b0;
      for (bit_i = 7; bit_i >= 0; bit_i = bit_i - 1) begin
        i_mosi = tx[bit_i];
        #(half);
        sampled = o_miso;
        i_sclk = 1'b1;
        got[bit_i] = sampled;
        #2;
        if (o_miso !== sampled) begin
          $display("ERROR hold: MISO changed right after rising edge (bit %0d)", bit_i);
          errors = errors + 1;
        end
        #(half - 2);
        i_sclk = 1'b0;
      end
      #(half);
      i_cs_n = 1'b1;
    end
  endtask

  task check_transfer(input [7:0] tx, input real half, input real gap);
    begin
      transfer(tx, half, rx);
      if (rx !== expected) begin
        $display("ERROR @%0t: half=%0.1f ns sent %02h got %02h expected %02h",
                 $time, half, tx, rx, expected);
        errors = errors + 1;
      end
      expected = tx;
      #(gap);
      if (o_irq !== 1'b1) begin
        $display("ERROR @%0t: o_irq not set after byte %02h", $time, tx);
        errors = errors + 1;
      end
    end
  endtask

  task run_rate(input real mhz, input integer count);
    real half;
    integer n, errors_at_start;
    begin
      half = 500.0 / mhz;
      errors_at_start = errors;
      for (n = 0; n < count; n = n + 1)
        check_transfer($random, half, 300.0);
      $display("%6.2f MHz: %0d bytes, %0d errors", mhz, count, errors - errors_at_start);
    end
  endtask

  // On the chip GSR clears every flop at configuration; Verilog starts them at
  // X. The power-on-reset counter has no reset of its own, and the SCLK-domain
  // bit counters are cleared by a rising edge on CS, so give them both a start.
  // NETLIST: simulating the synthesized netlist, whose Gowin cell models
  // already start at their INIT value.
  initial begin
`ifndef NETLIST
    dut.por_cnt = 0;
`endif
    i_cs_n = 1'b0;
    #1 i_cs_n = 1'b1;
  end

  initial begin
    if ($test$plusargs("vcd")) begin
      $dumpfile("tb_top.vcd");
      $dumpvars(0, tb_top);
    end

    // Let the power-on-reset counter release.
    #20000;

    // The IRQ clears once the next transaction asserts CS.
    check_transfer(8'h41, 50.0, 300.0);
    if (leds[0] !== 1'b1) begin
      $display("ERROR: LED not on after 0x41"); errors = errors + 1;
    end
    fork
      check_transfer(8'h40, 50.0, 300.0);
      begin
        #300;
        if (o_irq !== 1'b0) begin
          $display("ERROR: o_irq not cleared by CS"); errors = errors + 1;
        end
      end
    join
    if (leds[0] !== 1'b0) begin
      $display("ERROR: LED not off after 0x40"); errors = errors + 1;
    end

    run_rate(1.0, 64);
    run_rate(2.0, 64);
    run_rate(4.0, 64);
    run_rate(8.0, 64);
    run_rate(10.0, 64);
    run_rate(16.0, 64);
    run_rate(25.0, 64);

    if (errors == 0) $display("PASS");
    else $display("FAIL: %0d errors", errors);
    $finish;
  end
endmodule
