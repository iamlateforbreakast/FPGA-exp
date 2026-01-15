open Hardcaml
open Signal

module I = struct
  type 'a t = {
    clk   : 'a;
    clear : 'a;
    start : 'a; (* Trigger the init sequence *)
  } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    mosi : 'a;
    sclk : 'a;
    dc   : 'a; (* Data/Command select *)
    cs   : 'a; (* Chip Select *)
  } [@@deriving sexp_of, hardcaml]
end

let create (i : Signal.t I.t) =
  let open Always in
  
  (* Initialization Sequence for SH1106 *)
  let init_cmds = [| 
    0xAE; (* Display Off *)
    0x02; (* Set Low Column Address *)
    0x10; (* Set High Column Address *)
    0x40; (* Set Start Line *)
    0x81; 0xCF; (* Set Contrast *)
    0xA1; (* Segment Remap *)
    0xC8; (* COM Output Scan Direction *)
    0xA6; (* Normal Display *)
    0xAF; (* Display On *)
  |] in

  let counter = Reg_spec.override (Reg_spec.create ~clk:i.clk ()) ~clear:i.clear in
  let state = State_machine.create (module struct
    type t = IDLE | SEND_CMD | WAIT_SPI [@@deriving compare, enumerate, sexp_of]
  end) counter in

  let cmd_idx = Variable.wire ~default:(zero 4) in
  let dc_reg = Variable.wire ~default:gnd in
  
  compile [
    state.switch [
      IDLE, [
        if_ i.start [
          state.set_next SEND_CMD;
          cmd_idx <--. 0;
        ] []
      ];
      SEND_CMD, [
        (* Logic to load init_cmds[cmd_idx] into an SPI shifter *)
        dc_reg <-- gnd; 
        state.set_next WAIT_SPI;
      ];
      WAIT_SPI, [
        (* Wait for SPI module 'done' signal *)
        if_ (cmd_idx.value ==:. (Array.length init_cmds - 1)) [
          state.set_next IDLE;
        ] [
          cmd_idx <-- (cmd_idx.value +:. 1);
          state.set_next SEND_CMD;
        ]
      ]
    ]
  ];

  { O.mosi = ...; sclk = ...; dc = dc_reg.value; cs = ... }
