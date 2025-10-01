(* vesa.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
    type 'a t =
      { clock :    'a [@bits 1]  (* Need to be called clock for simulation *)
      ; i_resetn :  'a [@bits 1]
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
    let reset = ~:(i.i_resetn) in
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:reset () in
    let col_counter = reg_fb reg_sync_spec ~enable:vdd ~width:12
    ~f:(fun c -> mux2 (c >=:. (h_total_time - 1)) (zero 12) (c +:. 1)) in

   let row_counter = reg_fb reg_sync_spec ~enable:vdd ~width:12
    ~f:(fun c -> mux2 ((c >=:. (h_total_time - 1)) &: (c >=:. (v_total_time - 1))) (zero 12)
                     (mux2 (c >=:. (h_total_time - 1)) (c +:. 1) c)) in
    let row_addr_int = col_counter -:. (h_sync_time + h_bporch_time + h_lborder_time ) in
    let col_addr_int = row_counter -:. (v_sync_time + v_bporch_time + v_tborder_time ) in
    (* wire v_en = (row_addr_int < v_addr_time); *)
    (* wire h_en = (col_addr_int < h_addr_time); *)
    let v_en = row_addr_int <:. v_addr_time in
    let h_en = col_addr_int <:. h_addr_time in

    let column = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:12
      ~f:(fun c -> mux2 (c <:. (640 * 2 - 1)) (c +:. 1)(zero 12)) in
    let _dbg_counter = Signal.(column -- "dbg_column") in
    
    let row = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:12
      ~f:(fun c -> mux2 (c <:. (480 * 2 - 1)) (c +:. 1)(zero 12)) in

    (* vsync <= (row_counter < v_sync_time); *)
    let vsync = row_counter <:. v_sync_time in

    (* hsync <= (col_counter < h_sync_time - 1); *)
    let hsync = col_counter <:. (h_sync_time - 1) in

    (* data_en <= v_en & h_en; *)
    let data_en = v_en &: h_en in

    (* row <= row_addr_int[10:0]; *)
    {O.o_column = column; O.o_row = row; o_vsync = vsync; o_hsync = hsync; o_data_en = data_en}


  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
      H.hierarchical ~scope ~name:"vesa" ~instance:"inst" create i

end
