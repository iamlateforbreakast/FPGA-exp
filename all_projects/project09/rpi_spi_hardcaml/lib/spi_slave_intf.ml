(* spi_slave_intf.ml *)
open Hardcaml

(* Interface shared by the two SPI slave implementations, so [Top] can be
   built with either:

   - [Spi_slave_sclk]: shift registers clocked by SCLK; runs at 10 MHz+.
   - [Spi_slave_oversampled]: SCLK oversampled by the 27 MHz system clock;
     limited to about 2 MHz.

   Both are mode-0, MSB first, and return the byte received in the previous
   transaction on MISO. Only [Spi_slave_sclk] supports bursts of several bytes
   under one chip select; [Spi_slave_oversampled] sends 0x00 after the first.
   [rx_data]/[rx_valid] and [irq] are in the system-clock domain. *)
module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; reset : 'a [@bits 1]
    ; sclk  : 'a [@bits 1]
    ; cs_n  : 'a [@bits 1]
    ; mosi  : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { miso     : 'a [@bits 1]
    ; irq      : 'a [@bits 1]
    ; rx_data  : 'a [@bits 8]
    ; rx_valid : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

module type S = sig
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
