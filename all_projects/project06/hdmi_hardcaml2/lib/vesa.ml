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
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:i_reset () in
    let column = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:11
      ~f:(fun c -> mux2 (c <:. (X.clk_div * 2 - 1)) (c +:. 1)(zero 16)) in
    let _dbg_counter = Signal.(counter -- "dbg_column") in
    let row = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:11 
      ~f:(fun c -> mux2 (c <:. (X.clk_div * 2 - 1)) (c +:. 1)(zero 16)) in
    let vsync = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:1
      ~f:(fun c -> mux2 (c <:. (X.clk_div * 2 - 1)) (c +:. 1)(zero 16)) in
    let _dbg_counter = Signal.(counter -- "dbg_vsync") in
    let hsync = reg_fb reg_sync_spec 
      ~enable:vdd 
      ~width:1
      ~f:(fun c -> mux2 (c <:. (X.clk_div * 2 - 1)) (c +:. 1)(zero 16)) in
    let _dbg_counter = Signal.(counter -- "dbg_hsync") in
    {O.o_column = column; O.o_row = row; o_vsync = vsync; o_hsync = hsync; o_data_en = gnd}
  
end

