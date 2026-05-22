open Base
open Hardcaml
open Signal

(* Public system bus interfaces *)
module I = struct
  type 'a t = {
    clk       : 'a;
    resetn    : 'a;
    mem_ready : 'a;
    mem_rdata : 'a; [@bits 32]
  } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    trap      : 'a;
    mem_valid : 'a;
    mem_instr : 'a;
    mem_addr  : 'a; [@bits 32]
    mem_wdata : 'a; [@bits 32]
    mem_wstrb : 'a; [@bits 4]
  } [@@deriving sexp_of, hardcaml]
end

(* Internal microarchitectural register tracker *)
module Cpu_state = struct
  type 'a t = {
    pc        : 'a; [@bits 32]
    ir        : 'a; [@bits 32]
    fsm       : 'a; [@bits 3]
    rs1_data  : 'a; [@bits 32]
    rs2_data  : 'a; [@bits 32]
    alu_buf   : 'a; [@bits 32]
  } [@@deriving sexp_of, hardcaml]
end

(* Named structural definitions for the control FSM *)
module Fsm = struct
  let fetch      = const_int ~width:3 0
  let decode     = const_int ~width:3 1
  let exec_alu   = const_int ~width:3 2
  let exec_mem   = const_int ~width:3 3
  let mem_wait   = const_int ~width:3 4
  let writeback  = const_int ~width:3 5
end
