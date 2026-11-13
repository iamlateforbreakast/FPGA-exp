(* spi_slave_sclk.ml *)
open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

(* Mode-0 (CPOL=0, CPHA=0) SPI slave, MSB first, 8-bit bytes. CS may stay low
   for a burst of any length: the bit counters wrap every eight bits.

   The shift registers are clocked by SCLK itself rather than by oversampling
   it with the 27 MHz system clock, as [Spi_slave_oversampled] does. There a
   two-flop synchronizer and an edge detector sit between a falling SCLK edge
   and the next MISO bit (~150 ns), which caps the link at about 2 MHz.
   Clocked by SCLK, MISO follows a falling edge after one flop and the pad
   delay, so the link runs at 10 MHz and beyond.

   SCLK domain:
   - rising edge: shift MOSI in; on the eighth bit store the byte in
     [rx_byte] and flip [done_toggle].
   - falling edge: advance MISO through [rx_byte], i.e. the byte received in
     the *previous* transaction. Before the first falling edge MISO is
     [rx_byte]'s MSB, so the first bit is on the wire as soon as CS asserts.
   - CS high holds both bit counters in (asynchronous) reset.

   MISO returning the previous byte is a one-byte pipeline: the first
   transaction after reset returns 0x00. It gives both ends a simple way to
   verify the link (and to measure round-trip latency) without needing the
   master to know anything about the slave's state. Because the byte being
   sent is the byte last received, MISO never has to cross clock domains.

   System-clock domain: only [done_toggle] crosses, through a two-flop
   synchronizer. A change in it means [rx_byte] holds a new byte, which stays
   put for at least the eight SCLK periods of the next transfer - far longer
   than the synchronizer delay - so it is safe to copy into [rx_data] then.
   [irq] sets with [rx_valid] and clears when CS asserts for the next
   transaction, so the Pi can poll a GPIO instead of guessing when a transfer
   is done. *)
module Make (_ : Config.S) = struct
  module I = Spi_slave_intf.I
  module O = Spi_slave_intf.O

  let create (_scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let open Always in

    (* SCLK domain. *)
    let sclk_rise = Reg_spec.create ~clock:input.sclk ~reset:input.cs_n () in
    let sclk_fall = Reg_spec.override sclk_rise ~clock_edge:Edge.Falling in
    let sclk_keep = Reg_spec.create ~clock:input.sclk ~reset:input.reset () in

    let bit_cnt = Variable.reg sclk_rise ~width:3 in
    let rx_shift = Variable.reg sclk_rise ~width:7 in
    let rx_byte = Variable.reg sclk_keep ~width:8 in
    let done_toggle = Variable.reg sclk_keep ~width:1 in
    let tx_cnt = Variable.reg sclk_fall ~width:3 in
    let tx_bit = Variable.reg sclk_fall ~width:1 in

    let received = concat_msb [ rx_shift.value; input.mosi ] in
    compile [
      bit_cnt <-- bit_cnt.value +:. 1;
      rx_shift <-- select received 6 0;
      when_ (bit_cnt.value ==:. 7) [
        rx_byte <-- received;
        done_toggle <-- ~:(done_toggle.value);
      ];
      (* After falling edge n (n = 1..7) MISO carries bit 7-n. The value
         loaded on the eighth falling edge is never shown: tx_cnt wraps to 0
         and MISO goes back to the MSB. *)
      tx_cnt <-- tx_cnt.value +:. 1;
      tx_bit <-- mux tx_cnt.value
                   (List.map (bit rx_byte.value) [ 6; 5; 4; 3; 2; 1; 0; 7 ]);
    ];
    let miso = mux2 (tx_cnt.value ==:. 0) (msb rx_byte.value) tx_bit.value in

    (* System-clock domain. *)
    let spec = Reg_spec.create ~clock:input.clock ~clear:input.reset () in

    let sync2 value =
      let stage1 = Variable.reg spec ~width:1 in
      let stage2 = Variable.reg spec ~width:1 in
      compile [ stage1 <-- value; stage2 <-- stage1.value ];
      stage2.value
    in

    let toggle = sync2 done_toggle.value in
    let cs_n = sync2 input.cs_n in

    let toggle_d = Variable.reg spec ~width:1 in
    let cs_n_d = Variable.reg spec ~width:1 in
    compile [ toggle_d <-- toggle; cs_n_d <-- cs_n ];

    let byte_done = toggle ^: toggle_d.value in
    let cs_fall = ~:cs_n &: cs_n_d.value in

    let rx_data = Variable.reg spec ~width:8 in
    let rx_valid = Variable.reg spec ~width:1 in
    let irq = Variable.reg spec ~width:1 in

    compile [
      rx_valid <-- byte_done;
      when_ byte_done [ rx_data <-- rx_byte.value ];
      (* A byte completing wins over the next CS assertion in the same cycle. *)
      when_ cs_fall [ irq <-- gnd ];
      when_ byte_done [ irq <-- vdd ];
    ];

    { O.miso
    ; O.irq = irq.value
    ; O.rx_data = rx_data.value
    ; O.rx_valid = rx_valid.value
    }

  let hierarchical scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"spi_slave" ~instance:"inst1" create i
end
