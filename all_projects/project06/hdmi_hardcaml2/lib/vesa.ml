(* vesa.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
    type 'a t =
      { clock :    'a [@bits 1]  (* Need to be called clock for simulation *)
      ; i_reset :  'a [@bits 1]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_column :  'a [@bits 11]
      ; o_row :  'a [@bits 11]
      ; o_vsync :    'a [@bits 1]
      ; o_hsync :    'a [@bits 1]
      ; o_data_en : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  let create (_scope: Scope.t) (i: _ I.t) : _ O.t =
    let open Signal in
    let h_sync_time = 40 in
    let h_bporch_time = 220 in
    let h_fporch_time = 110 in
    let h_lborder_time = 0 in
    let h_rborder_time = 0 in
    let h_addr_time = 1280 in
    let v_sync_time = 5 in
    let v_bporch_time = 20 in
    let v_fporch_time = 5 in
    let v_tborder_time = 0 in
    let v_bborder_time = 0 in
    let v_addr_time = 720 in

    let h_total_time = h_sync_time + h_bporch_time + h_fporch_time + h_lborder_time + h_rborder_time + h_addr_time in
    let v_total_time = v_sync_time + v_bporch_time + v_fporch_time + v_tborder_time + v_bborder_time + v_addr_time in
    
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:i.i_reset () in
    let column = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:11
      ~f:(fun c -> mux2 (c <:. (640 * 2 - 1)) (c +:. 1)(zero 11)) in
    let _dbg_counter = Signal.(column -- "dbg_column") in
    
    let row = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:11 
      ~f:(fun c -> mux2 (c <:. (480 * 2 - 1)) (c +:. 1)(zero 11)) in
      
    

    let col_counter = reg_fb reg_sync_spec
      ~enable:vdd
      ~width:11
      ~f:(fun c -> mux2 (c <:. (h_total_time-1))(c +:. 1)(zero 11)) in
    let row_counter = reg_fb reg_sync_spec
      ~enable:vdd
      ~width:11
      ~f:(fun c -> mux2 (c <:. (v_total_time-1))(c +:. 1)(zero 11)) in
    
    let row_addr_int = col_counter -:. (h_sync_time + h_bporch_time + h_lborder_time ) in
    let col_addr_int = row_counter -:. (v_sync_time + v_bporch_time + v_tborder_time ) in

    (* column <= col_addr_int[10:0]; *)
    let vsync = reg reg_sync_spec 
      ~enable:vdd 
      ~width:1
      ~f:(fun c -> mux2 (c <:. (X.clk_div * 2 - 1)) (c +:. 1)(zero 16)) in
    let _dbg_counter = Signal.(vsync -- "dbg_vsync") in

    (* hsync <= (col_counter < h_sync_time - 1); *)
    let hsync = reg reg_sync_spec 
      ~enable:vdd 
      ~width:1
      ~f:(fun c -> mux2 (c <:. (X.clk_div * 2 - 1)) (c +:. 1)(zero 16)) in
    let _dbg_counter = Signal.(hsync -- "dbg_hsync") in
    
    {O.o_column = column; O.o_row = row; o_vsync = vsync; o_hsync = hsync; o_data_en = gnd}




  
    
    row <= row_addr_int[10:0];
    data_en <= v_en & h_en;
    vsync <= (row_counter < v_sync_time);
end

