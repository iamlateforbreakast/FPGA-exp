open Hardcaml
open Signal

module SPI_I = struct
  type 'a t = {
    clk   : 'a;
    clear : 'a;
    data_in : 'a; [@bits 8]
    start   : 'a;
  } [@@deriving sexp_of, hardcaml]
end

module SPI_O = struct
  type 'a t = {
    mosi : 'a;
    sclk : 'a;
    busy : 'a;
    done_ : 'a;
  } [@@deriving sexp_of, hardcaml]
end

let spi_master (i : Signal.t SPI_I.t) =
  let spec = Reg_spec.create ~clk:i.clk ~clear:i.clear () in
  
  (* Internal Registers *)
  let shift_reg = Always.Variable.reg spec ~width:8 in
  let bit_cnt   = Always.Variable.reg spec ~width:4 in
  let running   = Always.Variable.reg spec ~width:1 in
  let sclk_reg  = Always.Variable.reg spec ~width:1 in

  Always.(compile [
    if_ (i.start &&: ~:(running.value)) [
      shift_reg <-- i.data_in;
      bit_cnt   <--. 0;
      running   <-- gnd;
    ] [
      if_ running.value [
        (* Toggle clock and shift logic *)
        sclk_reg <-- ~:(sclk_reg.value);
        if_ (sclk_reg.value) [ (* Falling edge of SCLK *)
            shift_reg <-- (shift_reg.value @: [gnd]); (* Shift Left *)
            bit_cnt   <-- (bit_cnt.value +:. 1);
        ] [];
        
        if_ (bit_cnt.value ==:. 8) [
          running <-- gnd;
        ] [];
      ] []
    ]
  ]);

  { SPI_O.
    mosi = msb shift_reg.value;
    sclk = sclk_reg.value;
    busy = running.value;
    done_ = ~:(running.value);
  }
