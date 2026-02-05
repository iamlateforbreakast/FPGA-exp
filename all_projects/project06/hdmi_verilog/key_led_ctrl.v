module key_led_ctrl (
    input            I_clk,      
    input            I_rst_n,    
    input            I_key,      
    output     [4:0] O_led   
);
    
  reg [4:0] led_r;
  assign    O_led = ~led_r;

  reg [1:0] key_sync;
  wire      key_pos;       

  always @(posedge I_clk or negedge I_rst_n) begin
    if (!I_rst_n)          key_sync <= 2'b00;
    else                   key_sync <= { key_sync[0], I_key };
  end

  assign key_pos = key_sync[0] & ~key_sync[1];

 
  reg [2:0] press_cnt;
  reg       dir;

  always @(posedge I_clk or negedge I_rst_n) begin
    if (!I_rst_n) begin
      press_cnt <= 3'd0;
      dir       <= 1'b1;
    end
    else if (key_pos) begin
      if (dir) begin
        
        if (press_cnt == 3'd5) begin
          dir       <= 1'b0;     
          press_cnt <= 3'd4;     
        end
        else begin
          press_cnt <= press_cnt + 1;
        end
      end
      else begin
  
        if (press_cnt == 3'd0) begin
          dir       <= 1'b1;    
          press_cnt <= 3'd1;     
        end
        else begin
          press_cnt <= press_cnt - 1;
        end
      end
    end
  end

  // led_r = (1 << press_cnt) - 1
  //  press_cnt=0 → 00000  
  //           1 → 00001  
  //           2 → 00011  
  //           3 → 00111  
  //           4 → 01111  
  //           5 → 11111
  always @(posedge I_clk or negedge I_rst_n) begin
    if (!I_rst_n)
      led_r <= 5'b00000;
    else
      led_r <= ( (5'd1 << press_cnt) - 1 );
  end

endmodule