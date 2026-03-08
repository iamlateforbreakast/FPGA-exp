(* oled_sim.ml *)

(* ── SSD1306 constants ───────────────────────────────────────────────── *)
let screen_w = 128
let screen_h = 64
let pixel_scale = 8          (* each OLED pixel → 8×8 SDL pixels *)
let win_w = screen_w * pixel_scale
let win_h = screen_h * pixel_scale

(* OLED phosphor colours *)
let colour_on  = (0xFF, 0xA5, 0x00)   (* amber *)
let colour_off = (0x10, 0x08, 0x00)   (* very dark amber *)

(* ── SSD1306 model ───────────────────────────────────────────────────── *)
type dc_mode = Command | Data

type ssd1306 =
  { framebuffer : bool array          (* 128×64 flat, row-major *)
  ; mutable page          : int       (* 0-7  *)
  ; mutable col           : int       (* 0-127 *)
  ; mutable dc            : dc_mode
  (* multi-byte command state *)
  ; mutable cmd_pending   : int option  (* Some remaining_arg_bytes *)
  ; mutable last_cmd      : int
  }

let make_ssd1306 () =
  { framebuffer   = Array.make (screen_w * screen_h) false
  ; page          = 0
  ; col           = 0
  ; dc            = Command
  ; cmd_pending   = None
  ; last_cmd      = 0
  }

(* Write one data byte into the framebuffer at current page/col *)
let write_data_byte oled byte =
  let base_row = oled.page * 8 in
  for bit = 0 to 7 do
    let row = base_row + bit in
    if row < screen_h then begin
      let idx = row * screen_w + oled.col in
      oled.framebuffer.(idx) <- (byte lsr bit) land 1 = 1
    end
  done;
  oled.col <- (oled.col + 1) land 0x7F

(* Minimal SSD1306 command decoder — extend as needed *)
let handle_command oled byte =
  match oled.cmd_pending with
  | Some 0 ->
    (* Last argument of a multi-byte command *)
    (match oled.last_cmd with
     | 0x21 -> oled.col  <- byte          (* Set Column Address end *)
     | 0x22 -> oled.page <- byte          (* Set Page Address end   *)
     | _    -> ());
    oled.cmd_pending <- None
  | Some n ->
    (* Intermediate argument *)
    (match oled.last_cmd with
     | 0x21 -> oled.col  <- byte          (* Set Column Address start *)
     | 0x22 -> oled.page <- byte          (* Set Page Address start   *)
     | _    -> ());
    oled.cmd_pending <- Some (n - 1)
  | None ->
    oled.last_cmd <- byte;
    (match byte with
     | 0x00..0x0F ->                       (* Set lower col nibble *)
       oled.col <- (oled.col land 0xF0) lor (byte land 0x0F)
     | 0x10..0x1F ->                       (* Set higher col nibble *)
       oled.col <- (oled.col land 0x0F) lor ((byte land 0x0F) lsl 4)
     | 0xB0..0xB7 ->                       (* Set page address *)
       oled.page <- byte land 0x07
     | 0x21 ->                             (* Set column address (3 bytes) *)
       oled.cmd_pending <- Some 1
     | 0x22 ->                             (* Set page address (3 bytes) *)
       oled.cmd_pending <- Some 1
     | 0xAE -> ()                          (* Display off — ignore in sim *)
     | 0xAF -> ()                          (* Display on  — ignore in sim *)
     | 0x40..0x7F ->                       (* Set display start line *)
       ()
     | _ -> ()                             (* Ignore unimplemented cmds *)
    )

let ssd1306_receive oled ~dc byte =
  (match dc with
   | Data    -> write_data_byte oled byte
   | Command -> handle_command  oled byte)

(* ── SPI slave shift register ────────────────────────────────────────── *)
type spi_slave =
  { mutable shift    : int
  ; mutable bit_cnt  : int
  ; mutable prev_sclk: int
  ; mutable active   : bool   (* CS low = active *)
  }

let make_spi_slave () =
  { shift = 0; bit_cnt = 0; prev_sclk = 0; active = false }

(* Returns Some byte when a full 8-bit word has been received *)
let spi_tick slave ~cs ~sclk ~mosi =
  slave.active <- (cs = 0);
  if not slave.active then begin
    slave.bit_cnt  <- 0;
    slave.shift    <- 0;
    slave.prev_sclk <- sclk;
    None
  end else begin
    let rising = slave.prev_sclk = 0 && sclk = 1 in
    slave.prev_sclk <- sclk;
    if rising then begin
      slave.shift   <- (slave.shift lsl 1) lor (mosi land 1);
      slave.bit_cnt <- slave.bit_cnt + 1;
      if slave.bit_cnt = 8 then begin
        let byte = slave.shift land 0xFF in
        slave.shift   <- 0;
        slave.bit_cnt <- 0;
        Some byte
      end else None
    end else None
  end

(* ── SDL2 renderer ───────────────────────────────────────────────────── *)
module Sdl = Tsdl.Sdl

let sdl_check msg = function
  | Error (`Msg e) -> failwith (Printf.sprintf "SDL %s: %s" msg e)
  | Ok v -> v

let render renderer texture (fb : bool array) =
  (* Upload pixels into the texture *)
  let pitch = win_w * 4 in
  let pixels = Bigarray.(Array1.create int8_unsigned c_layout (win_h * pitch)) in
  for row = 0 to screen_h - 1 do
    for col = 0 to screen_w - 1 do
      let on = fb.(row * screen_w + col) in
      let (r, g, b) = if on then colour_on else colour_off in
      for dy = 0 to pixel_scale - 1 do
        for dx = 0 to pixel_scale - 1 do
          let px = col * pixel_scale + dx in
          let py = row * pixel_scale + dy in
          let base = py * pitch + px * 4 in
          pixels.{base + 0} <- r;
          pixels.{base + 1} <- g;
          pixels.{base + 2} <- b;
          pixels.{base + 3} <- 0xFF
        done
      done
    done
  done;
  sdl_check "update_texture"
    (Sdl.update_texture texture None pixels pitch);
  sdl_check "render_clear"  (Sdl.render_clear renderer);
  sdl_check "render_copy"   (Sdl.render_copy renderer texture ());
  Sdl.render_present renderer

(* ── Main simulation loop ────────────────────────────────────────────── *)
module Sim = Hardcaml.Cyclesim
module Cs  = Hardcaml.Cyclesim.Port_list

(* Instantiate your SPI module here *)
module MySpi = Screen_spi.Make(struct
  (* fill in your Config.S fields *)
end)

let () =
  (* SDL init *)
  sdl_check "init" (Sdl.init Sdl.Init.video);
  let window =
    sdl_check "create_window"
      (Sdl.create_window "OLED Sim" ~x:100 ~y:100
         ~w:win_w ~h:win_h Sdl.Window.shown) in
  let renderer =
    sdl_check "create_renderer"
      (Sdl.create_renderer window ~index:(-1)
         ~flags:Sdl.Renderer.accelerated) in
  let texture =
    sdl_check "create_texture"
      (Sdl.create_texture renderer Sdl.Pixel.format_rgb888
         Sdl.Texture.access_streaming ~w:win_w ~h:win_h) in

  (* Hardcaml sim *)
  let scope = Hardcaml.Scope.create ~flatten_design:true () in
  let sim   = Sim.create (MySpi.create scope) in
  let i     = Sim.inputs  sim in
  let o     = Sim.outputs sim in

  let oled  = make_ssd1306  () in
  let slave = make_spi_slave () in

  (* DC pin — wire this from your testbench driving logic *)
  let dc_pin = ref 0 in

  let running = ref true in
  let event   = Sdl.Event.create () in

  (* Helper: read 1-bit port *)
  let bit port = Hardcaml.Bits.to_int !port land 1 in

  (* Drive a test pattern — replace with your real stimulus *)
  let cycle_count = ref 0 in
  let send_byte byte dc =
    dc_pin := dc;
    i.data_in    := Hardcaml.Bits.of_int ~width:8 byte;
    i.data_valid := Hardcaml.Bits.vdd;
    Sim.cycle sim;
    i.data_valid := Hardcaml.Bits.gnd;
    (* Wait for ready *)
    let timeout = ref 0 in
    while Hardcaml.Bits.to_int !(o.ready) = 0 && !timeout < 10000 do
      Sim.cycle sim;
      incr timeout;
      incr cycle_count;
      (* SPI slave sampling *)
      (match spi_tick slave
               ~cs:(bit o.cs) ~sclk:(bit o.sclk) ~mosi:(bit o.mosi) with
       | Some b ->
         ssd1306_receive oled ~dc:(if !dc_pin = 1 then Data else Command) b
       | None -> ())
    done
  in

  (* Send a simple init sequence + fill screen *)
  send_byte 0xAE 0;   (* display off *)
  send_byte 0xAF 0;   (* display on  *)
  (* Fill all pages with 0xAA checkerboard *)
  for page = 0 to 7 do
    send_byte (0xB0 lor page) 0;   (* set page *)
    send_byte 0x00 0;               (* col low  *)
    send_byte 0x10 0;               (* col high *)
    for _ = 0 to 127 do
      send_byte 0xAA 1              (* data     *)
    done
  done;

  (* Event + render loop *)
  while !running do
    while Sdl.poll_event (Some event) do
      (match Sdl.Event.(get event typ |> enum) with
       | `Quit -> running := false
       | _ -> ())
    done;
    render renderer texture oled.framebuffer;
    Sdl.delay 16
  done;

  Sdl.destroy_texture  texture;
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window   window;
  Sdl.quit ()