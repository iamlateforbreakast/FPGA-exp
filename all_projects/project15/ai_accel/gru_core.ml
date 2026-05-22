(** GRU Inference Accelerator — Tang Nano 20K
    
    This revision wires the BRAM weight reads fully into the state machine.

    Data path per token step
    ========================

    Each gate (r, z, n) requires TWO dot products:
      (a) W_i · x   — input weight matrix times input embedding
      (b) W_h · h   — hidden weight matrix times hidden state

    Both are computed by the same MAC unit in sequence (2 × hidden_size cycles
    each), then the bias is added and the result is passed through the LUT.

    The final hidden update and FC argmax follow.

    BRAM layout
    ===========
    We use HardCaml's synchronous ROM primitive (mux of constants).
    In Gowin synthesis this infers block RAM automatically for large tables.

    Weight ROMs (all INT8, flat row-major):
      embed_rom   : [vocab_size  × hidden_size]  — embedding matrix
      w_ir_rom    : [hidden_size × hidden_size]  — r gate, input weights
      w_hr_rom    : [hidden_size × hidden_size]  — r gate, hidden weights
      w_iz_rom    : [hidden_size × hidden_size]  — z gate, input weights
      w_hz_rom    : [hidden_size × hidden_size]  — z gate, hidden weights
      w_in_rom    : [hidden_size × hidden_size]  — n gate, input weights
      w_hn_rom    : [hidden_size × hidden_size]  — n gate, hidden weights
      fc_rom      : [vocab_size  × hidden_size]  — output projection
      bias_rom    : [6 × hidden_size]            — all bias vectors packed

    Addressing
    ==========
    During gate computation for output unit j, column k:
      weight address = j * hidden_size + k    (walks columns for unit j)
    The counter register is k (0..hidden_size-1).
    j is held in a separate unit register (0..hidden_size-1 for gates,
    0..vocab_size-1 for FC).

    Activation source
    =================
    During W_i · x:  activation = embed_reg[k]  (loaded in LOAD_X)
    During W_h · h:  activation = h_reg[k]       (current hidden state)
*)

open Hardcaml
open Signal

(* =========================================================================
   Parameters
   ========================================================================= *)

module P = struct
  let hidden  = 128
  let vocab   = 62    (* must match vocab.json at synthesis time *)
  let dbits   = 8     (* INT8 *)
  let abits   = 32    (* accumulator *)

  (* Flat ROM sizes *)
  let embed_size = vocab  * hidden
  let gate_size  = hidden * hidden
  let fc_size    = vocab  * hidden
  let bias_size  = 6      * hidden

  (* Bias slot indices (must match train.py packing order) *)
  let b_ir = 0
  let b_hr = 1
  let b_iz = 2
  let b_hz = 3
  let b_in = 4
  let b_hn = 5
end

(* =========================================================================
   Port interface
   ========================================================================= *)

module I = struct
  type 'a t =
    { clock    : 'a [@bits 1]
    ; reset    : 'a [@bits 1]
    ; spi_sclk : 'a [@bits 1]
    ; spi_mosi : 'a [@bits 1]
    ; spi_cs_n : 'a [@bits 1]
    ; start    : 'a [@bits 1]
    ; char_in  : 'a [@bits 8]   (* vocab index provided by SPI / Pi *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { spi_miso  : 'a [@bits 1]
    ; token_out : 'a [@bits 8]
    ; done_     : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

(* =========================================================================
   Weight ROM builder
   Builds a synchronous 8-bit ROM from an int array.
   Gowin synthesis infers BRAM when the table is large enough (>= 512 bits).
   ========================================================================= *)

let make_rom ~clock ~(init : int array) ~addr =
  let entries =
    Array.map (fun v -> Signal.of_int ~width:8 (v land 0xFF)) init
  in
  reg (Reg_spec.create ~clock ()) ~enable:vdd
    (mux addr (Array.to_list entries))

(* Mutable weight arrays — zero by default.
   Call inject_weights before building the circuit to fill from .bin files. *)
let embed_init = Array.make P.embed_size 0
let w_ir_init  = Array.make P.gate_size  0
let w_hr_init  = Array.make P.gate_size  0
let w_iz_init  = Array.make P.gate_size  0
let w_hz_init  = Array.make P.gate_size  0
let w_in_init  = Array.make P.gate_size  0
let w_hn_init  = Array.make P.gate_size  0
let fc_init    = Array.make P.fc_size    0
let bias_init  = Array.make P.bias_size  0

(* =========================================================================
   Activation LUTs  (256-entry synchronous ROMs, 1-cycle latency)
   ========================================================================= *)

let sigmoid_lut ~clock ~x =
  let init = Array.init 256 (fun i ->
    let xi = Float.of_int (i - 128) /. 16.0 in
    let s  = 1.0 /. (1.0 +. Float.exp (-. xi)) in
    Int.of_float (s *. 255.0))
  in
  make_rom ~clock ~init ~addr:(uresize x 8)

let tanh_lut ~clock ~x =
  let init = Array.init 256 (fun i ->
    let xi = Float.of_int (i - 128) /. 16.0 in
    let t  = Float.tanh xi in
    Int.of_float ((t +. 1.0) /. 2.0 *. 255.0))
  in
  make_rom ~clock ~init ~addr:(uresize x 8)

(* =========================================================================
   MAC unit  (INT8 × INT8 → INT32 accumulator)
   ========================================================================= *)

module Mac = struct
  let create ~clock ~reset ~enable ~clear ~a ~b =
    let spec = Reg_spec.create ~clock ~reset () in
    let acc  = Always.Variable.reg spec ~enable:vdd ~width:P.abits in
    Always.(compile [
      if_ clear [
        acc <--. 0
      ] @@ elif enable [
        acc <-- acc.value +: (sresize a P.abits *+ sresize b P.abits)
      ] []
    ]);
    acc.value
end

(* =========================================================================
   Saturating clamp: INT32 → INT8  (returns dbits-wide signal)
   ========================================================================= *)

let clamp8 x =
  mux2 (x <+. (-128)) (Signal.of_int ~width:P.abits (-128))
    (mux2 (x >+. 127) (Signal.of_int ~width:P.abits 127) x)
  |> fun v -> v.:[(P.dbits - 1, 0)]

(* =========================================================================
   GRU cell — main circuit
   ========================================================================= *)

let create _scope (i : _ I.t) =
  let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

  (* -----------------------------------------------------------------------
     State encoding
     IDLE(0) LOAD_X(1) MAC_WX(2) MAC_WH(3) ADD_BIAS(4)
     STORE_G(5) UPDATE_H(6) FC_MAC(7) OUTPUT(8)
     ----------------------------------------------------------------------- *)
  let sw = 4 in  (* 4 bits covers 0..8 *)
  let s v = Signal.of_int ~width:sw v in
  let st_idle     = s 0 in
  let st_load_x   = s 1 in
  let st_mac_wx   = s 2 in
  let st_mac_wh   = s 3 in
  let st_add_bias = s 4 in
  let st_store_g  = s 5 in
  let st_update_h = s 6 in
  let st_fc_mac   = s 7 in
  let st_output   = s 8 in

  (* -----------------------------------------------------------------------
     Control registers
     ----------------------------------------------------------------------- *)
  let state      = Always.Variable.reg spec ~enable:vdd ~width:sw  in
  let unit_j     = Always.Variable.reg spec ~enable:vdd ~width:8   in  (* output unit index *)
  let col_k      = Always.Variable.reg spec ~enable:vdd ~width:8   in  (* dot-product column *)
  let gate_sel   = Always.Variable.reg spec ~enable:vdd ~width:2   in  (* 0=r 1=z 2=n *)
  let phase      = Always.Variable.reg spec ~enable:vdd ~width:1   in  (* 0=W_i·x  1=W_h·h *)
  let done_r     = Always.Variable.reg spec ~enable:vdd ~width:1   in
  let token_r    = Always.Variable.reg spec ~enable:vdd ~width:8   in
  let fc_max_val = Always.Variable.reg spec ~enable:vdd ~width:P.abits in
  let fc_max_idx = Always.Variable.reg spec ~enable:vdd ~width:8   in

  (* -----------------------------------------------------------------------
     Data registers
     ----------------------------------------------------------------------- *)
  let mk_array n =
    Array.init n (fun _ -> Always.Variable.reg spec ~enable:vdd ~width:P.dbits)
  in
  let embed_reg = mk_array P.hidden in
  let h_reg     = mk_array P.hidden in
  let r_reg     = mk_array P.hidden in
  let z_reg     = mk_array P.hidden in
  let n_reg     = mk_array P.hidden in

  let read_array arr =
    mux col_k.value (Array.to_list (Array.map (fun v -> v.Always.Variable.value) arr))
  in
  let read_array_j arr =
    mux unit_j.value (Array.to_list (Array.map (fun v -> v.Always.Variable.value) arr))
  in

  (* -----------------------------------------------------------------------
     BRAM address calculation
     addr = row * 128 + col  (row is unit_j or char_in, col is col_k)
     hidden=128 so multiply by 128 = shift left 7.
     We build a 16-bit address then truncate.
     ----------------------------------------------------------------------- *)
  let addr_hh =
    (* unit_j × 128 + col_k *)
    let base = uresize unit_j.value 16 @: Signal.zero 7 in
    (base +: uresize col_k.value 16) |> fun a -> a.:[(12, 0)]
    (* 13 bits covers 128×128=16384 entries *)
  in
  let addr_embed =
    let base = uresize i.char_in 16 @: Signal.zero 7 in
    (base +: uresize col_k.value 16) |> fun a -> a.:[(12, 0)]
  in
  let addr_fc =
    let base = uresize unit_j.value 16 @: Signal.zero 7 in
    (base +: uresize col_k.value 16) |> fun a -> a.:[(12, 0)]
  in
  (* bias address: slot × 128 + unit_j *)
  let gate_phase = gate_sel.value @: phase.value in  (* 3-bit *)
  let bias_slot =
    mux gate_phase [
      Signal.of_int ~width:4 P.b_ir;   (* 00 0 *)
      Signal.of_int ~width:4 P.b_hr;   (* 00 1 *)
      Signal.of_int ~width:4 P.b_iz;   (* 01 0 *)
      Signal.of_int ~width:4 P.b_hz;   (* 01 1 *)
      Signal.of_int ~width:4 P.b_in;   (* 10 0 *)
      Signal.of_int ~width:4 P.b_hn;   (* 10 1 *)
      Signal.of_int ~width:4 0;
      Signal.of_int ~width:4 0;
    ]
  in
  let addr_bias =
    let base = uresize bias_slot 16 @: Signal.zero 7 in
    (base +: uresize unit_j.value 16) |> fun a -> a.:[(9, 0)]
  in

  (* -----------------------------------------------------------------------
     Weight ROMs
     ----------------------------------------------------------------------- *)
  let embed_q = make_rom ~clock:i.clock ~init:embed_init ~addr:addr_embed in
  let w_ir_q  = make_rom ~clock:i.clock ~init:w_ir_init  ~addr:addr_hh   in
  let w_hr_q  = make_rom ~clock:i.clock ~init:w_hr_init  ~addr:addr_hh   in
  let w_iz_q  = make_rom ~clock:i.clock ~init:w_iz_init  ~addr:addr_hh   in
  let w_hz_q  = make_rom ~clock:i.clock ~init:w_hz_init  ~addr:addr_hh   in
  let w_in_q  = make_rom ~clock:i.clock ~init:w_in_init  ~addr:addr_hh   in
  let w_hn_q  = make_rom ~clock:i.clock ~init:w_hn_init  ~addr:addr_hh   in
  let fc_q    = make_rom ~clock:i.clock ~init:fc_init    ~addr:addr_fc   in
  let bias_q  = make_rom ~clock:i.clock ~init:bias_init  ~addr:addr_bias  in

  (* -----------------------------------------------------------------------
     MAC input muxes
     weight: pick the right gate ROM based on gate_sel × phase
     act:    embedding (W_i phase) or hidden state (W_h phase)
     ----------------------------------------------------------------------- *)
  let mac_weight =
    mux gate_phase [
      w_ir_q; w_hr_q;   (* gate r: phases 0,1 *)
      w_iz_q; w_hz_q;   (* gate z: phases 0,1 *)
      w_in_q; w_hn_q;   (* gate n: phases 0,1 *)
      fc_q;   fc_q;     (* padding — overridden for FC_MAC below *)
    ]
  in
  (* During FC_MAC we always use fc_q and h_reg *)
  let mac_w_final =
    mux2 (state.value ==: st_fc_mac) fc_q mac_weight
  in
  let mac_act =
    mux2
      ((state.value ==: st_fc_mac) |: (phase.value ==: Signal.of_int ~width:1 1))
      (read_array h_reg)
      (read_array embed_reg)
  in

  (* -----------------------------------------------------------------------
     MAC instantiation
     enable: high while we are in a dot-product state (after 1-cycle ROM latency)
     clear:  pulse when we leave a dot-product state
     ----------------------------------------------------------------------- *)
  let in_mac_state =
    (state.value ==: st_mac_wx) |:
    (state.value ==: st_mac_wh) |:
    (state.value ==: st_fc_mac)
  in
  (* Delay enable by 1 cycle to account for synchronous ROM read latency *)
  let mac_enable = reg spec ~enable:vdd in_mac_state in
  (* Clear the accumulator when entering a new dot-product.
     We pulse clear on the ADD_BIAS / STORE_G / OUTPUT transitions. *)
  let mac_clear =
    (state.value ==: st_add_bias) |:
    (state.value ==: st_store_g)  |:
    (state.value ==: st_output)
  in

  let mac_acc = Mac.create
    ~clock:i.clock ~reset:i.reset
    ~enable:mac_enable
    ~clear:mac_clear
    ~a:mac_w_final
    ~b:mac_act
  in

  (* -----------------------------------------------------------------------
     Activation: add bias then pass through LUT
     acc_plus_bias is the pre-activation; it is valid in ADD_BIAS state.
     The LUT output (lut_out) is valid one cycle after (captured in STORE_G).
     ----------------------------------------------------------------------- *)
  let acc_plus_bias = mac_acc +: sresize bias_q P.abits in
  let clamped       = clamp8 acc_plus_bias in
  let sig_out       = sigmoid_lut ~clock:i.clock ~x:clamped in
  let tanh_out      = tanh_lut    ~clock:i.clock ~x:clamped in
  (* gate n uses tanh; r and z use sigmoid *)
  let lut_out = mux2 (gate_sel.value ==:. 2) tanh_out sig_out in

  (* -----------------------------------------------------------------------
     UPDATE_H blending:  h'[j] = round((255 - z[j]) * n[j] + z[j] * h[j]) >> 8
     All values in [0,255] unsigned after LUT.
     ----------------------------------------------------------------------- *)
  let z_j = read_array_j z_reg in
  let n_j = read_array_j n_reg in
  let h_j = read_array_j h_reg in
  let h_new_j =
    let z16 = uresize z_j 16 in
    let n16 = uresize n_j 16 in
    let h16 = uresize h_j 16 in
    let blend = ((Signal.of_int ~width:16 255 -: z16) *: n16)
                +: (z16 *: h16) in
    blend.:[(15, 8)]   (* >> 8 *)
  in

  (* -----------------------------------------------------------------------
     State machine
     ----------------------------------------------------------------------- *)
  let hidden_done = col_k.value  ==:. (P.hidden - 1) in
  let vocab_done  = unit_j.value ==:. (P.vocab  - 1) in
  let units_done  = unit_j.value ==:. (P.hidden - 1) in

  Always.(compile [
    if_ i.reset [
      state      <-- st_idle;
      unit_j     <--. 0;
      col_k      <--. 0;
      gate_sel   <--. 0;
      phase      <--. 0;
      done_r     <--. 0;
      token_r    <--. 0;
      fc_max_val <--. 0;
      fc_max_idx <--. 0;
      Array.iter (fun r -> r <--. 0) h_reg;
      Array.iter (fun r -> r <--. 0) embed_reg;
      Array.iter (fun r -> r <--. 0) r_reg;
      Array.iter (fun r -> r <--. 0) z_reg;
      Array.iter (fun r -> r <--. 0) n_reg;
    ] [
      done_r <--. 0;

      switch state.value [

        (* IDLE ------------------------------------------------------------ *)
        st_idle, [
          if_ i.start [
            state      <-- st_load_x;
            unit_j     <--. 0;
            col_k      <--. 0;
            gate_sel   <--. 0;
            phase      <--. 0;
            fc_max_val <--. 0;
            fc_max_idx <--. 0;
          ] []
        ];

        (* LOAD_X: present embed addresses; latch results with 1-cycle delay *)
        (* We iterate col_k 0..hidden-1; embed_q for col_k is valid next cycle *)
        st_load_x, [
          if_ hidden_done [
            state  <-- st_mac_wx;
            col_k  <--. 0;
          ] [
            col_k  <-- col_k.value +:. 1;
          ]
        ];

        (* MAC_WX: dot-product of input weight row × embedding              *)
        (* col_k walks 0..hidden-1; MAC reads weight[unit_j, col_k] and
           embed_reg[col_k] each cycle (with 1-cycle enable delay)          *)
        st_mac_wx, [
          if_ hidden_done [
            state <-- st_mac_wh;
            col_k <--. 0;
          ] [
            col_k <-- col_k.value +:. 1;
          ]
        ];

        (* MAC_WH: dot-product of hidden weight row × hidden state          *)
        st_mac_wh, [
          if_ hidden_done [
            state <-- st_add_bias;
            col_k <--. 0;
          ] [
            col_k <-- col_k.value +:. 1;
          ]
        ];

        (* ADD_BIAS: acc is valid; bias_q is presented; LUT takes 1 cycle  *)
        (* We wait 2 cycles: cycle 0 presents input, cycle 1 lut_out valid *)
        st_add_bias, [
          if_ (col_k.value ==:. 1) [
            state <-- st_store_g;
            col_k <--. 0;
          ] [
            col_k <-- col_k.value +:. 1;
          ]
        ];

        (* STORE_G: lut_out is valid — write to gate register for unit_j   *)
        st_store_g, [
          (* Decide what to do next *)
          if_ units_done [
            unit_j <--. 0;
            col_k  <--. 0;
            (* All units finished for this gate+phase *)
            if_ (gate_sel.value ==:. 2) [
              (* Finished gate n (all 3 gates done) → UPDATE_H *)
              state    <-- st_update_h;
              gate_sel <--. 0;
              phase    <--. 0;
            ] @@ elif (phase.value ==:. 0) [
              (* Finished W_i phase → start W_h phase *)
              phase <--. 1;
              state <-- st_mac_wx;
            ] [
              (* Finished W_h phase → next gate *)
              phase    <--. 0;
              gate_sel <-- gate_sel.value +:. 1;
              state    <-- st_mac_wx;
            ]
          ] [
            unit_j <-- unit_j.value +:. 1;
            col_k  <--. 0;
            state  <-- st_mac_wx;
          ]
        ];

        (* UPDATE_H: one cycle per unit, h_reg[unit_j] ← h_new_j          *)
        st_update_h, [
          if_ units_done [
            state  <-- st_fc_mac;
            unit_j <--. 0;
            col_k  <--. 0;
          ] [
            unit_j <-- unit_j.value +:. 1;
          ]
        ];

        (* FC_MAC: project hidden → vocab logits, track argmax             *)
        (* For each vocab unit j: dot fc[j,:] · h over hidden cycles       *)
        st_fc_mac, [
          if_ hidden_done [
            if_ vocab_done [
              state <-- st_output;
            ] [
              unit_j <-- unit_j.value +:. 1;
              col_k  <--. 0;
            ]
          ] [
            col_k <-- col_k.value +:. 1;
          ]
        ];

        (* OUTPUT: latch argmax, pulse done_ -------------------------------- *)
        st_output, [
          token_r <-- fc_max_idx.value;
          done_r  <--. 1;
          state   <-- st_idle;
        ];

      ]
    ]
  ]);

  (* -----------------------------------------------------------------------
     Gate register write-enable
     During STORE_G, write lut_out into r/z/n_reg[unit_j].
     One Always block per unit, guarded by unit_j match.
     ----------------------------------------------------------------------- *)
  let store_g_active = state.value ==: st_store_g in
  Array.iteri (fun j _ ->
    let sel = store_g_active &: (unit_j.value ==:. j) in
    Always.(compile [
      if_ sel [
        if_   (gate_sel.value ==:. 0) [ r_reg.(j) <-- lut_out ]
        @@ elif (gate_sel.value ==:. 1) [ z_reg.(j) <-- lut_out ]
        [                               n_reg.(j) <-- lut_out ]
      ] []
    ])
  ) r_reg;

  (* -----------------------------------------------------------------------
     Hidden state update
     During UPDATE_H, h_reg[unit_j] ← h_new_j
     ----------------------------------------------------------------------- *)
  let update_h_active = state.value ==: st_update_h in
  Array.iteri (fun j _ ->
    let sel = update_h_active &: (unit_j.value ==:. j) in
    Always.(compile [ if_ sel [ h_reg.(j) <-- h_new_j ] [] ])
  ) h_reg;

  (* -----------------------------------------------------------------------
     Embedding register load
     During LOAD_X, embed_q (with 1-cycle latency) is written into
     embed_reg[col_k_d1] on the following cycle.
     ----------------------------------------------------------------------- *)
  let col_k_d1  = reg spec ~enable:vdd col_k.value in
  let load_x_d1 = reg spec ~enable:vdd (state.value ==: st_load_x) in
  Array.iteri (fun j _ ->
    let sel = load_x_d1 &: (col_k_d1 ==:. j) in
    Always.(compile [ if_ sel [ embed_reg.(j) <-- embed_q ] [] ])
  ) embed_reg;

  (* -----------------------------------------------------------------------
     FC argmax accumulation
     At the end of each FC dot-product (hidden_done in FC_MAC state,
     delayed by 1 to let MAC settle) check if mac_acc > fc_max_val.
     ----------------------------------------------------------------------- *)
  let fc_mac_active  = state.value ==: st_fc_mac in
  let fc_end_d1      = reg spec ~enable:vdd (fc_mac_active &: hidden_done) in
  let unit_j_d1      = reg spec ~enable:vdd unit_j.value in
  Always.(compile [
    if_ fc_end_d1 [
      if_ (mac_acc >+ fc_max_val.value) [
        fc_max_val <-- mac_acc;
        fc_max_idx <-- unit_j_d1;
      ] []
    ] []
  ]);

  (* -----------------------------------------------------------------------
     SPI slave — mode 0, 8-bit frames, MSB first out
     Pi sends a byte (char index), FPGA shifts out token_out.
     ----------------------------------------------------------------------- *)
  let sclk_d1   = reg spec ~enable:vdd i.spi_sclk in
  let spi_rise  = i.spi_sclk  &: (~: sclk_d1) in
  let spi_fall  = (~: i.spi_sclk) &: sclk_d1 in
  let spi_shift = Always.Variable.reg spec ~enable:vdd ~width:8 in
  let spi_rx    = Always.Variable.reg spec ~enable:vdd ~width:8 in
  let spi_cnt   = Always.Variable.reg spec ~enable:vdd ~width:4 in

  Always.(compile [
    if_ i.spi_cs_n [
      (* Deasserted: preload output register *)
      spi_shift <-- token_r.value;
      spi_cnt   <--. 0;
    ] [
      if_ spi_rise [
        spi_rx  <-- (lsbs spi_rx.value @: i.spi_mosi);
        spi_cnt <-- spi_cnt.value +:. 1;
      ] [];
      if_ spi_fall [
        spi_shift <-- (lsbs spi_shift.value @: gnd);
      ] [];
    ]
  ]);

  (* When a full byte received, the Pi can inspect spi_rx; for now we
     expose it only as a debug hook — the main data path uses char_in. *)
  ignore spi_rx;
  ignore spi_cnt;

  (* -----------------------------------------------------------------------
     Outputs
     ----------------------------------------------------------------------- *)
  { O.spi_miso  = msb spi_shift.value
  ; token_out   = token_r.value
  ; done_       = done_r.value
  }

(* =========================================================================
   Weight injection (called before circuit build to load from .bin files)
   ========================================================================= *)

let inject_weights (ws : Weight_loader.weight_tensor list) =
  let find name =
    match List.find_opt (fun (w : Weight_loader.weight_tensor) -> w.name = name) ws with
    | Some w -> w.data
    | None   -> failwith ("inject_weights: missing tensor: " ^ name)
  in
  let blit src src_off dst = Array.blit src src_off dst 0 (Array.length dst) in
  let blit_section src off dst len =
    Array.blit src (off * len) dst 0 len
  in
  (* Embeddings *)
  blit (find "embed") 0 embed_init;
  (* GRU weight_ih is packed as [3*H, H]:
       rows 0..H-1   = W_ir
       rows H..2H-1  = W_iz
       rows 2H..3H-1 = W_in                                               *)
  let wih = find "gru_weight_ih" in
  let whh = find "gru_weight_hh" in
  blit_section wih 0 w_ir_init P.gate_size;
  blit_section wih 1 w_iz_init P.gate_size;
  blit_section wih 2 w_in_init P.gate_size;
  blit_section whh 0 w_hr_init P.gate_size;
  blit_section whh 1 w_hz_init P.gate_size;
  blit_section whh 2 w_hn_init P.gate_size;
  (* FC *)
  blit (find "fc_weight") 0 fc_init;
  (* Biases: pack all 6 into bias_init *)
  let bih = find "gru_bias_ih" in
  let bhh = find "gru_bias_hh" in
  Array.blit bih 0              bias_init (P.b_ir * P.hidden) P.hidden;
  Array.blit bhh 0              bias_init (P.b_hr * P.hidden) P.hidden;
  Array.blit bih P.hidden       bias_init (P.b_iz * P.hidden) P.hidden;
  Array.blit bhh P.hidden       bias_init (P.b_hz * P.hidden) P.hidden;
  Array.blit bih (2 * P.hidden) bias_init (P.b_in * P.hidden) P.hidden;
  Array.blit bhh (2 * P.hidden) bias_init (P.b_hn * P.hidden) P.hidden

(* =========================================================================
   Simulation harness
   ========================================================================= *)

module Sim = Cyclesim.With_interface(I)(O)

let run_sim ?(weights_dir = "") () =
  if weights_dir <> "" then begin
    let ws = Weight_loader.load_all weights_dir in
    inject_weights ws;
    Printf.printf "Loaded weights from: %s\n%!" weights_dir
  end else
    Printf.printf "Smoke-test: running with zero weights\n%!";

  let scope = Scope.create ~flatten_design:false () in
  let sim   = Sim.create (create scope) in
  let inp   = Cyclesim.inputs  sim in
  let out   = Cyclesim.outputs sim in

  Printf.printf "Resetting...\n%!";
  inp.reset := Bits.vdd;
  for _ = 1 to 4 do Cyclesim.cycle sim done;
  inp.reset := Bits.gnd;
  Cyclesim.cycle sim;

  (* Token step: send char index 10 (arbitrary for smoke test) *)
  inp.char_in := Bits.of_int ~width:8 10;
  inp.start   := Bits.vdd;
  Cyclesim.cycle sim;
  inp.start   := Bits.gnd;

  Printf.printf "Running...\n%!";
  (* Cycle budget per token:
       LOAD_X:    128 cycles
       Per gate:  2 phases × 128 MAC + 2 LUT + 1 store = ~260 cycles × 3 gates = ~780
       UPDATE_H:  128 cycles
       FC_MAC:    62 units × 128 MAC cycles = 7936 cycles
       Total:     ~9000 cycles                                             *)
  let max_cycles = 12_000 in
  let finished   = ref false in
  let cycle_cnt  = ref 0 in
  while not !finished && !cycle_cnt < max_cycles do
    Cyclesim.cycle sim;
    incr cycle_cnt;
    if Bits.to_int !(out.done_) = 1 then begin
      Printf.printf "Done at cycle %d. Token index: %d\n%!"
        !cycle_cnt (Bits.to_int !(out.token_out));
      finished := true
    end
  done;
  if not !finished then
    Printf.printf "Timed out after %d cycles (check state machine)\n%!" max_cycles

let () =
  let weights_dir =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else ""
  in
  run_sim ~weights_dir ()
