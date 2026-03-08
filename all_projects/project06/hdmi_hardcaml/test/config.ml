(* Config.ml — shared configuration signature and a concrete 720p instance *)

module type S = sig
  (* Horizontal timing (pixels) *)
  val h_total  : int
  val h_sync   : int
  val h_bporch : int
  val h_res    : int

  (* Vertical timing (lines) *)
  val v_total  : int
  val v_sync   : int
  val v_bporch : int
  val v_res    : int

  (* Sync polarities: true = active-high pulse *)
  val hs_pol : bool
  val vs_pol : bool
end

(* 720p @ 60 Hz  (1280x720, pixel clock ~74.25 MHz)
   Timings from CEA-861-D                                *)
module Res_720p : S = struct
  let h_total  = 1650
  let h_sync   = 40
  let h_bporch = 220
  let h_res    = 1280

  let v_total  = 750
  let v_sync   = 5
  let v_bporch = 20
  let v_res    = 720

  let hs_pol = true
  let vs_pol = true
end

(* 480p @ 60 Hz  (640x480) – cheaper to simulate (fewer cycles) *)
module Res_480p : S = struct
  let h_total  = 800
  let h_sync   = 96
  let h_bporch = 48
  let h_res    = 640

  let v_total  = 525
  let v_sync   = 2
  let v_bporch = 33
  let v_res    = 480

  let hs_pol = false   (* 480p uses negative polarity *)
  let vs_pol = false
end