(* Test_config.ml — concrete [Project06_lib.Config.S] instances for the
   test-benches.

   These must implement the *library's* Config.S (not a local copy), so the
   functors [Test_pattern.Make], [Dvi_encoder.Make] and [Dvi_tx.Make] can be
   applied to them directly.  [clk_fre] / [pattern] are only consumed by
   [Leds], which the tests do not exercise; [is_simulation] is true here. *)

module type S = Project06_lib.Config.S

(* 720p @ 60 Hz  (1280x720, pixel clock 74.25 MHz)
   Timings from CEA-861-D — these match hdmi_verilog/video_top.v. *)
module Res_720p : S = struct
  let clk_fre = 74_250_000

  (* Horizontal timing (pixels) *)
  let h_total  = 1650
  let h_sync   = 40
  let h_bporch = 220
  let h_res    = 1280

  (* Vertical timing (lines) *)
  let v_total  = 750
  let v_sync   = 5
  let v_bporch = 20
  let v_res    = 720

  (* Sync polarities: true = active-high pulse *)
  let hs_pol = true
  let vs_pol = true

  let is_simulation = true
  let pattern = [ 0; 1; 2; 3; 4; 5; 6; 7 ]
end

(* 480p @ 60 Hz  (640x480, pixel clock 25.175 MHz) – cheaper to simulate *)
module Res_480p : S = struct
  let clk_fre = 25_175_000

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

  let is_simulation = true
  let pattern = [ 0; 1; 2; 3; 4; 5; 6; 7 ]
end
