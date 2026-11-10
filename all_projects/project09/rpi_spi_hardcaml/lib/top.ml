(* top.ml *)
open Hardcaml
open Hardcaml.Signal

module type Config = Config.S

module Make (X : Config.S) = struct
  module I = struct
    type 'a t =
      { clock  : 'a [@bits 1] (* must be called clock for simulation *)
      ; i_reset : 'a [@bits 1]
      ; i_sclk  : 'a [@bits 1]
      ; i_cs_n  : 'a [@bits 1]
      ; i_mosi  : 'a [@bits 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { o_miso : 'a [@bits 1]
      ; o_irq  : 'a [@bits 1]
      ; leds   : 'a [@bits X.led_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module MySpi = Spi_slave.Make (X)

  let create (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let open Always in
    (* Power-on reset: GSR clears every flop at the end of configuration, then
       this saturating counter fills and releases [por]. See project08's
       top.ml for why the design does not rely on the reset button alone. *)
    let por_cnt =
      reg_fb (Reg_spec.create ~clock:i.clock ()) ~enable:vdd ~width:X.por_bits
        ~f:(fun c -> mux2 (msb c) c (c +:. 1))
    in
    let por = ~:(msb por_cnt) in
    let reset = X.normalize_reset i.i_reset |: por in
    let spec = Reg_spec.create ~clock:i.clock ~clear:reset () in

    let spi =
      MySpi.hierarchical scope
        MySpi.I.
          { clock = i.clock
          ; reset
          ; sclk = i.i_sclk
          ; cs_n = i.i_cs_n
          ; mosi = i.i_mosi
          }
    in

    (* A byte with 01 in its top two bits drives the LEDs, so the Pi can prove
       that writes reach the fabric as well as that reads come back. Bytes
       outside that range are still echoed on MISO, which keeps a pure
       loopback test possible. *)
    let leds = Variable.reg spec ~width:X.led_width in
    compile
      [ when_ spi.rx_valid
          [ when_ (select spi.rx_data 7 6 ==:. 0b01)
              [ leds <-- select spi.rx_data (X.led_width - 1) 0 ]
          ]
      ];

    { O.o_miso = spi.miso; O.o_irq = spi.irq; O.leds = leds.value }
end
