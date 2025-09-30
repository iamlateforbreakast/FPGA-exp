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
  {O.o_column = zero 11; O.o_row = zero 11; o_vsync = gnd; o_hsync = gnd; o_data_en = gnd}
  
end

