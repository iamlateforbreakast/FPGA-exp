(* spi_slave_oversampled.ml *)
open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

(* Mode-0 (CPOL=0, CPHA=0) SPI slave, MSB first, eight bits per chip select.

   SCLK is asynchronous to the 27 MHz system clock, so SCLK, CS_N and MOSI each
   go through a two-flop synchronizer and edges are detected on the
   synchronized versions. On every rising SCLK edge the slave samples MOSI; on
   every falling edge it advances MISO.

   The synchronizer and edge detector put about four system clocks (~150 ns)
   between a falling SCLK edge and the next MISO bit, so the Pi, which samples
   MISO half an SCLK period later, reads stale bits above about 2 MHz.
   [Spi_slave_sclk] avoids this by clocking its shift registers from SCLK.

   MISO returns the byte received in the *previous* transaction: TX is loaded
   from [rx_data] when CS asserts, so the first transaction after reset returns
   0x00. That one-byte pipeline is intentional - it gives both ends a simple
   way to verify the link (and to measure round-trip latency) without needing
   the master to know anything about the slave's state. *)
module Make (_ : Config.S) = struct
  module I = Spi_slave_intf.I
  module O = Spi_slave_intf.O

  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let open Always in
    let spec = Reg_spec.create ~clock:input.clock ~clear:input.reset () in

    let sync2 value =
      let stage1 = Variable.reg spec ~width:1 in
      let stage2 = Variable.reg spec ~width:1 in
      compile [ stage1 <-- value; stage2 <-- stage1.value ];
      stage2.value
    in

    let sclk = sync2 input.sclk in
    let cs_n = sync2 input.cs_n in
    let mosi = sync2 input.mosi in

    let sclk_d = Variable.reg spec ~width:1 in
    let cs_n_d = Variable.reg spec ~width:1 in
    compile [ sclk_d <-- sclk; cs_n_d <-- cs_n ];

    let sclk_rise = sclk &: ~:(sclk_d.value) in
    let sclk_fall = ~:(sclk) &: sclk_d.value in
    let cs_fall = ~:(cs_n) &: cs_n_d.value in
    let cs_active = ~:(cs_n) in

    let rx_shift = Variable.reg spec ~width:8 in
    let tx_shift = Variable.reg spec ~width:8 in
    let bit_cnt = Variable.reg spec ~width:3 in
    let rx_data = Variable.reg spec ~width:8 in
    let rx_valid = Variable.reg spec ~width:1 in
    let irq = Variable.reg spec ~width:1 in

    compile [
      (* Defaults; conditional assignments below take priority. *)
      rx_valid <-- gnd;
      irq <-- irq.value;
      when_ cs_fall [
        bit_cnt <--. 0;
        tx_shift <-- rx_data.value;
        irq <-- gnd;
      ];
      when_ (cs_active &: sclk_rise) [
        rx_shift <-- concat_msb [ select rx_shift.value 6 0; mosi ];
        if_ (bit_cnt.value ==:. 7) [
          rx_data <-- concat_msb [ select rx_shift.value 6 0; mosi ];
          rx_valid <-- vdd;
          irq <-- vdd;
          bit_cnt <--. 0;
        ] [
          bit_cnt <-- bit_cnt.value +:. 1;
        ];
      ];
      when_ (cs_active &: sclk_fall) [
        tx_shift <-- concat_msb [ select tx_shift.value 6 0; gnd ];
      ];
    ];

    { O.miso = msb tx_shift.value
    ; O.irq = irq.value
    ; O.rx_data = rx_data.value
    ; O.rx_valid = rx_valid.value
    }

  let hierarchical scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"spi_slave" ~instance:"inst1" create i
end
