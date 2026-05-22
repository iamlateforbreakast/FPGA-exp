module SoC_Top = struct
  let create ~clock ~clear (leds_out : Signal.t) =
    (* Core inputs state tracker *)
    let mem_ready = r_wire () in
    let mem_rdata = r_wire () in
    
    (* Core instantiation *)
    let cpu_inputs = Pico_inputs.{
      clk = clock;
      resetn = ~: clear; (* Gowin S1 button is usually active-low reset *)
      mem_ready = w_input mem_ready;
      mem_rdata = w_input mem_rdata;
    } in
    let cpu = make_picorv32 cpu_inputs in

    (* Simple Address Decoding Logic *)
    let is_ram  = (cpu.mem_addr_bits 31 16) ==:. 0x0000 in
    let is_gpio = (cpu.mem_addr_bits 31 16) ==:. 0x8000 in

    (* Synchronous RAM Block *)
    (* HardCaml infers memory blocks that Gowin EDA maps into native BRAM *)
    let ram_out = memory 2048
      ~write_port:{ 
        write_clock = clock; 
        write_address = cpu.mem_addr_bits 12 2; 
        write_enable = cpu.mem_valid &: cpu.mem_wstrb <>:. 0 &: is_ram; 
        write_data = cpu.mem_wdata; 
      }
      ~read_port:{ 
        read_clock = clock; 
        read_address = cpu.mem_addr_bits 12 2; 
        read_enable = cpu.mem_valid &: is_ram; 
      }
     in

    (* GPIO Register Output Mapping *)
    let gpio_reg = reg ~clock ~clear 
      ~enable:(cpu.mem_valid &: (cpu.mem_wstrb ==:. 0xF) &: is_gpio) 
      cpu.mem_wdata 
    in

    (* Connect System Bus Logic back to Core inputs *)
    mem_ready <== (cpu.mem_valid); (* 1-cycle execution response *)
    mem_rdata <== mux2 is_gpio gpio_reg ram_out;

    (* Drive external physical Pins *)
    gpio_reg.[5,0] (* Route lower 6 bits to Tang Nano onboard LEDs *)
end
