open Base
open Hardcaml
open Signal
open Interfaces

module Make (Combinational : sig end) = struct

  let create (inputs : Signal.t I.t) =
    let clock = inputs.clk in
    let clear = ~: (inputs.resetn) in (* Convert active-low resetn to active-high clear *)

    (* Instantiate feedback loop wires for sequential internal registers *)
    let rec_reg_in = Cpu_state.In_wires.make () in
    let regs = Cpu_state.reg ~clock ~clear rec_reg_in in

    (* ========================================================================= *)
    (* 1. INSTRUCTION FIELD DECODING                                             *)
    (* ========================================================================= *)
    let opcode = select regs.ir 6 0 in
    let rd     = select regs.ir 11 7 in
    let funct3 = select regs.ir 14 12 in
    let rs1    = select regs.ir 19 15 in
    let rs2    = select regs.ir 24 20 in
    let funct7 = select regs.ir 31 25 in

    (* RV32I Immediate Bitfield Reconstructions *)
    let imm_i = sreg (select regs.ir 31 20) in
    let imm_s = sreg (cat [select regs.ir 31 25; select regs.ir 11 7]) in
    let imm_b = sreg (cat [select regs.ir 31; select regs.ir 7; select regs.ir 30 25; select regs.ir 11 8; zero 1]) in
    let imm_u = cat [select regs.ir 31 12; zero 12] in
    let imm_j = sreg (cat [select regs.ir 31; select regs.ir 19 12; select regs.ir 20; select regs.ir 30 21; zero 1]) in

    (* Opcode Decoders *)
    let is_lui      = opcode ==:. 0x37 in
    let is_auipc    = opcode ==:. 0x17 in
    let is_jal      = opcode ==:. 0x6F in
    let is_jalr     = opcode ==:. 0x67 in
    let is_branch   = opcode ==:. 0x63 in
    let is_load     = opcode ==:. 0x03 in
    let is_store    = opcode ==:. 0x23 in
    let is_alu_imm  = opcode ==:. 0x13 in
    let is_alu_reg  = opcode ==:. 0x33 in

    (* ========================================================================= *)
    (* 2. THE MULTI-CYCLE CONTROL FSM & ROUTING LOGIC                            *)
    (* ========================================================================= *)
    let next_fsm    = w_wire 3 in
    let mem_valid   = w_wire 1 in
    let mem_addr    = w_wire 32 in
    let mem_wdata   = w_wire 32 in
    let mem_wstrb   = w_wire 4 in
    let trap        = w_wire 1 in

    (* Default Wire Overrides *)
    let () =
      next_fsm  <== regs.fsm;
      mem_valid <== gnd;
      mem_addr  <== regs.pc;
      mem_wdata <== zeros 32;
      mem_wstrb <== zeros 4;
      trap      <== gnd;

      switch regs.fsm [
        (* FETCH State *)
        0, [
          mem_valid <== vdd;
          mem_addr  <== regs.pc;
          if_ inputs.mem_ready 
            [ next_fsm <== Fsm.decode ]
            [ next_fsm <== Fsm.fetch ]
        ];
        (* DECODE State *)
        1, [
          if_ (is_lui |: is_auipc |: is_jal)
            [ next_fsm <== Fsm.writeback ]
          (if_ (is_alu_imm |: is_alu_reg |: is_jalr |: is_branch)
            [ next_fsm <== Fsm.exec_alu ]
          (if_ (is_load |: is_store)
            [ next_fsm <== Fsm.exec_mem ]
            [ trap <== vdd; next_fsm <== Fsm.fetch ])) (* Trap on bad opcode *)
        ];
        (* EXEC_ALU State *)
        2, [
          if_ is_branch 
            [ next_fsm <== Fsm.fetch ]
            [ next_fsm <== Fsm.writeback ]
        ];
        (* EXEC_MEM State *)
        3, [
          mem_valid <== vdd;
          mem_addr  <== regs.alu_buf;
          if_ is_store [
            (* Calculate accurate byte-enable mask arrays for alignment *)
            switch (select regs.alu_buf 1 0) [
              0, [ mem_wstrb <== mux2 (funct3 ==:. 0) (const_string "0001") (mux2 (funct3 ==:. 1) (const_string "0011") (const_string "1111")); mem_wdata <== regs.rs2_data ];
              1, [ mem_wstrb <== const_string "0010"; mem_wdata <== (select regs.rs2_data 7 0) #: 8 ];
              2, [ mem_wstrb <== mux2 (funct3 ==:. 0) (const_string "0100") (const_string "1100"); mem_wdata <== (select regs.rs2_data 15 0) #: 16 ];
              3, [ mem_wstrb <== const_string "1000"; mem_wdata <== (select regs.rs2_data 7 0) #: 24 ];
            ]
          ] [
            mem_wstrb <== zeros 4
          ];
          next_fsm <== Fsm.mem_wait
        ];
        (* MEM_WAIT State *)
        4, [
          mem_valid <== vdd;
          mem_addr  <== regs.alu_buf;
          if_ inputs.mem_ready
            [ next_fsm <== mux2 is_load Fsm.writeback Fsm.fetch ]
            [ next_fsm <== Fsm.mem_wait ]
        ];
        (* WRITEBACK State *)
        5, [
          next_fsm <== Fsm.fetch
        ]
      ]
    in

    (* ========================================================================= *)
    (* 3. ARITHMETIC LOGIC UNIT (ALU) & JUMP RESOLUTIONS                          *)
    (* ========================================================================= *)
    let alu_op2  = mux2 is_alu_reg regs.rs2_data imm_i in
    let alu_out  = w_wire 32 in
    let sub_op   = is_alu_reg &: (select funct7 5 5) in
    
    let () =
      switch funct3 [
        0, [ alu_out <== mux2 sub_op (regs.rs1_data -: alu_op2) (regs.rs1_data +: alu_op2) ]; (* ADD / SUB *)
        1, [ alu_out <== regs.rs1_data <: (select alu_op2 4 0) ];                           (* SLL *)
        2, [ alu_out <== mux2 (regs.rs1_data <+ alu_op2) (ones 32) (zeros 32) ];            (* SLT *)
        3, [ alu_out <== mux2 (regs.rs1_data <* alu_op2) (ones 32) (zeros 32) ];            (* SLTU *)
        4, [ alu_out <== regs.rs1_data ^: alu_op2 ];                                        (* XOR *)
        5, [ alu_out <== mux2 (select funct7 5 5) (sra regs.rs1_data (select alu_op2 4 0)) (srl regs.rs1_data (select alu_op2 4 0)) ]; (* SRL / SRA *)
        6, [ alu_out <== regs.rs1_data |: alu_op2 ];                                        (* OR *)
        7, [ alu_out <== regs.rs1_data &: alu_op2 ];                                        (* AND *)
      ]
    in

    (* Evaluate Branch Conditions *)
    let branch_true = w_wire 1 in
    let () =
      switch funct3 [
        0, [ branch_true <== (regs.rs1_data ==: regs.rs2_data) ];  (* BEQ *)
        1, [ branch_true <== (regs.rs1_data <>: regs.rs2_data) ];  (* BNE *)
        4, [ branch_true <== (regs.rs1_data <+ regs.rs2_data) ];   (* BLT *)
        5, [ branch_true <== (regs.rs1_data >=+ regs.rs2_data) ];  (* BGE *)
        6, [ branch_true <== (regs.rs1_data <* regs.rs2_data) ];   (* BLTU *)
        7, [ branch_true <== (regs.rs1_data >=* regs.rs2_data) ];  (* BGEU *)
        default, [ branch_true <== gnd ]
      ]
    in

    (* ========================================================================= *)
    (* 4. DUAL-PORT INTERNAL REGISTER FILE SYSTEM                                *)
    (* ========================================================================= *)
    let reg_write_data = w_wire 32 in
    let reg_write_en   = (regs.fsm ==:. 5) &: (rd <>:. 0) in

    (* Handle Custom Mask Loading for Sized Memory Read Operations *)
    let () =
      if_ is_load [
        switch funct3 [
          0, [ reg_write_data <== sreg (select inputs.mem_rdata 7 0) ];   (* LB *)
          1, [ reg_write_data <== sreg (select inputs.mem_rdata 15 0) ];  (* LH *)
          2, [ reg_write_data <== inputs.mem_rdata ];                    (* LW *)
          4, [ reg_write_data <== ureg (select inputs.mem_rdata 7 0) ];   (* LBU *)
          5, [ reg_write_data <== ureg (select inputs.mem_rdata 15 0) ];  (* LHU *)
          default, [ reg_write_data <== inputs.mem_rdata ]
        ]
      ] [
        if_ is_lui [ reg_write_data <== imm_u ]
        (if_ is_auipc [ reg_write_data <== regs.pc +: imm_u ]
        (if_ (is_jal |: is_jalr) [ reg_write_data <== regs.pc +: const_int ~width:32 4 ]
        [ reg_write_data <== regs.alu_buf ]))
      ]
    in

    (* Synchronous Register File Inferences *)
    let rf_rs1 = memory 32
      ~write_port:{ write_clock=clock; write_address=rd; write_enable=reg_write_en; write_data=reg_write_data }
      ~read_port:{ read_clock=clock; read_address=rs1; read_enable=(regs.fsm ==:. 1) }
    in
    let rf_rs2 = memory 32
      ~write_port:{ write_clock=clock; write_address=rd; write_enable=reg_write_en; write_data=reg_write_data }
      ~read_port:{ read_clock=clock; read_address=rs2; read_enable=(regs.fsm ==:. 1) }
    in

    (* ========================================================================= *)
    (* 5. REGISTER STATE TRANSITIONS AND LOOP CLOSURES                          *)
    (* ========================================================================= *)
    let next_pc = w_wire 32 in
    let () =
      next_pc <== regs.pc;
      if_ (regs.fsm ==:. 5) [
        if_ is_jal  [ next_pc <== regs.pc +: imm_j ]
        (if_ is_jalr [ next_pc <== regs.alu_buf ])
        [ next_pc <== regs.pc +: const_int ~width:32 4 ]
      ] (if_ (regs.fsm ==:. 2 &: is_branch &: branch_true) [
        next_pc <== regs.pc +: imm_b
      ] [])
    in

    let next_ir = mux2 (regs.fsm ==:. 0 &: inputs.mem_ready) inputs.mem_rdata regs.ir in
    
    let next_alu_buf = w_wire 32 in
    let () =
      if_ (regs.fsm ==:. 1) [
        if_ (is_load |: is_store) [ next_alu_buf <== rf_rs1 +: (mux2 is_load imm_i imm_s) ]
        (if_ is_jalr [ next_alu_buf <== (rf_rs1 +: imm_i) &: (const_int ~width:32 0xFFFFFFFE) ])
        [ next_alu_buf <== alu_out ]
      ] [
        next_alu_buf <== alu_out
      ]
    in

    (* Commit structural outputs to latch the loop closure *)
    Cpu_state.In_wires.set rec_reg_in {
      pc       = next_pc;
      ir       = next_ir;
      fsm      = next_fsm;
      rs1_data = rf_rs1;
      rs2_data = rf_rs2;
      alu_buf  = next_alu_buf;
    };

    (* Route to outward physical interfaces *)
    O.{
      trap      = trap;
      mem_valid = mem_valid;
      mem_instr = (regs.fsm ==:. 0);
      mem_addr  = mem_addr;
      mem_wdata = mem_wdata;
      mem_wstrb = mem_wstrb;
    }
end
