/**
 * spw_spi_driver.h — Host-side C driver for the HardCaml SpaceWire SPI interface.
 *
 * Targets any MCU with a standard SPI master peripheral (RP2040, STM32, AVR, etc.).
 * Implement the three platform stubs at the bottom for your hardware.
 *
 * Protocol: 16-bit SPI mode 0 (CPOL=0, CPHA=0), MSB first.
 *   Frame: [R/nW(1)] [ADDR(7)] [DATA(8)]
 *
 * Usage example:
 *   spw_write(SPW_REG_CTRL, SPW_CTRL_LINK_EN | SPW_CTRL_TX_EN);
 *   while (!(spw_read(SPW_REG_STATUS) & SPW_STATUS_LINK_RUN));
 *   spw_send_byte(0x42);
 *   spw_send_eop();
 */

#ifndef SPW_SPI_DRIVER_H
#define SPW_SPI_DRIVER_H

#include <stdint.h>
#include <stdbool.h>

/* ── Register addresses ────────────────────────────────────────────── */
#define SPW_REG_CTRL        0x00u
#define SPW_REG_STATUS      0x01u
#define SPW_REG_TX_DATA     0x02u
#define SPW_REG_TX_CTRL     0x03u
#define SPW_REG_RX_DATA     0x04u
#define SPW_REG_RX_CTRL     0x05u
#define SPW_REG_TIMECODE    0x06u
#define SPW_REG_IRQ_EN      0x07u
#define SPW_REG_IRQ_STATUS  0x08u
#define SPW_REG_LINK_CFG    0x09u

/* ── CTRL register bits (0x00) ─────────────────────────────────────── */
#define SPW_CTRL_LINK_EN    (1u << 0)  /**< Enable link FSM             */
#define SPW_CTRL_LOOPBACK   (1u << 1)  /**< Internal loopback           */
#define SPW_CTRL_TX_EN      (1u << 2)  /**< Enable TX serialiser        */
#define SPW_CTRL_SOFT_RESET (1u << 3)  /**< W1C: soft reset link FSM    */

/* ── STATUS register bits (0x01) [RO] ─────────────────────────────── */
#define SPW_STATUS_LINK_STATE_MASK  (0x07u)
#define SPW_STATUS_LINK_STATE_SHIFT (0u)
#define SPW_STATUS_RX_AVAIL         (1u << 3)
#define SPW_STATUS_TX_READY         (1u << 4)
#define SPW_STATUS_DISCONNECT       (1u << 5)
#define SPW_STATUS_PARITY_ERR       (1u << 6)
#define SPW_STATUS_CREDIT_ERR       (1u << 7)

/** Link FSM state codes (STATUS bits [2:0]) */
#define SPW_LINK_ERROR_RESET    0u
#define SPW_LINK_ERROR_WAIT     1u
#define SPW_LINK_READY          2u
#define SPW_LINK_STARTED        3u
#define SPW_LINK_CONNECTING     4u
#define SPW_LINK_RUN            5u

/* ── TX_CTRL register bits (0x03) [WO] ────────────────────────────── */
#define SPW_TX_EOP          (1u << 0)  /**< Enqueue End-Of-Packet      */
#define SPW_TX_EEP          (1u << 1)  /**< Enqueue Error-End-Of-Packet*/
#define SPW_TX_TIMECODE     (1u << 2)  /**< Transmit current timecode  */

/* ── RX_CTRL register bits (0x05) [RO] ────────────────────────────── */
#define SPW_RX_IS_DATA      (1u << 0)
#define SPW_RX_IS_EOP       (1u << 1)
#define SPW_RX_IS_EEP       (1u << 2)
#define SPW_RX_IS_TIMECODE  (1u << 3)

/* ── IRQ_STATUS / IRQ_EN bits (0x08 / 0x07) ───────────────────────── */
#define SPW_IRQ_RX_AVAIL    (1u << 0)
#define SPW_IRQ_TX_EMPTY    (1u << 1)
#define SPW_IRQ_LINK_UP     (1u << 2)
#define SPW_IRQ_LINK_DOWN   (1u << 3)
#define SPW_IRQ_PARITY_ERR  (1u << 4)
#define SPW_IRQ_DISCONNECT  (1u << 5)
#define SPW_IRQ_CREDIT_ERR  (1u << 6)

/* ── LINK_CFG register bits (0x09) ────────────────────────────────── */
#define SPW_CFG_TX_DIV_MASK     (0x0Fu)        /**< TX clock divider [3:0]  */
#define SPW_CFG_RX_OS_MASK      (0x30u)        /**< RX oversample  [5:4]    */
#define SPW_CFG_RX_OS_4X        (0x00u << 4)
#define SPW_CFG_RX_OS_8X        (0x01u << 4)
#define SPW_CFG_RX_OS_16X       (0x02u << 4)
#define SPW_CFG_AUTO_START      (1u << 6)

/* ═══════════════════════════════════════════════════════════════════
   Platform stubs — implement these for your MCU
   ═══════════════════════════════════════════════════════════════════ */

/** Assert CS low before a transaction. */
static inline void spw_cs_assert(void);

/** De-assert CS high after a transaction. */
static inline void spw_cs_deassert(void);

/** Transfer one byte over SPI (send tx_byte, return received byte).
    Mode 0, MSB first, 8-bit.  Call twice per 16-bit frame. */
static inline uint8_t spw_spi_transfer(uint8_t tx_byte);

/* ═══════════════════════════════════════════════════════════════════
   Core register access functions
   ═══════════════════════════════════════════════════════════════════ */

/**
 * spw_write() — Write 8-bit value to a register.
 * Frame: [0][ADDR(7)][DATA(8)]  (R/nW = 0)
 */
static inline void spw_write(uint8_t addr, uint8_t data)
{
    uint8_t hi = ((addr & 0x7Fu));   /* R/nW=0, addr in bits [6:0] */
    spw_cs_assert();
    spw_spi_transfer(hi);
    spw_spi_transfer(data);
    spw_cs_deassert();
}

/**
 * spw_read() — Read 8-bit value from a register.
 * Frame: [1][ADDR(7)][0x00]   (R/nW = 1)
 * Returns the 8-bit register value clocked out on MISO.
 *
 * Note: due to the 1-cycle MISO pre-load latency, send a dummy read
 * first when switching read targets, or accept a 1-frame latency.
 */
static inline uint8_t spw_read(uint8_t addr)
{
    uint8_t hi = 0x80u | (addr & 0x7Fu);  /* R/nW=1 */
    spw_cs_assert();
    spw_spi_transfer(hi);
    uint8_t result = spw_spi_transfer(0x00u);
    spw_cs_deassert();
    return result;
}

/* ═══════════════════════════════════════════════════════════════════
   High-level SpaceWire helpers
   ═══════════════════════════════════════════════════════════════════ */

/** Start the SpaceWire link.  Blocks until the link reaches Run state
 *  or [timeout_ms] milliseconds elapse.  Returns true on success. */
static inline bool spw_link_start(uint32_t timeout_ms)
{
    spw_write(SPW_REG_CTRL, SPW_CTRL_LINK_EN | SPW_CTRL_TX_EN);
    uint32_t t = 0;
    while (t < timeout_ms * 10u) {          /* crude spin; replace with timer */
        uint8_t st = spw_read(SPW_REG_STATUS);
        if ((st & SPW_STATUS_LINK_STATE_MASK) == SPW_LINK_RUN)
            return true;
        for (volatile int d = 0; d < 100; d++);  /* ~0.1 ms delay stub */
        t++;
    }
    return false;
}

/** Stop the SpaceWire link and reset the link FSM. */
static inline void spw_link_stop(void)
{
    spw_write(SPW_REG_CTRL, SPW_CTRL_SOFT_RESET);
    spw_write(SPW_REG_CTRL, 0x00u);
}

/** Send one data byte.  Caller must check tx_ready or handle blocking. */
static inline bool spw_send_byte(uint8_t data)
{
    if (!(spw_read(SPW_REG_STATUS) & SPW_STATUS_TX_READY))
        return false;
    spw_write(SPW_REG_TX_DATA, data);
    return true;
}

/** Enqueue an End-Of-Packet marker. */
static inline void spw_send_eop(void)
{
    spw_write(SPW_REG_TX_CTRL, SPW_TX_EOP);
}

/** Enqueue an Error-End-Of-Packet marker. */
static inline void spw_send_eep(void)
{
    spw_write(SPW_REG_TX_CTRL, SPW_TX_EEP);
}

/** Send a complete packet from a buffer.  Returns false if TX not ready. */
static inline bool spw_send_packet(const uint8_t *buf, uint16_t len)
{
    for (uint16_t i = 0; i < len; i++) {
        /* Spin-wait for TX ready (add timeout for production code) */
        while (!(spw_read(SPW_REG_STATUS) & SPW_STATUS_TX_READY));
        spw_write(SPW_REG_TX_DATA, buf[i]);
    }
    spw_send_eop();
    return true;
}

/**
 * spw_recv_byte() — Pop one character from the RX FIFO.
 * Returns false if FIFO empty.
 * [out_data]  : received byte (valid when is_data is set).
 * [out_flags] : RX_CTRL bits — check SPW_RX_IS_* macros.
 */
static inline bool spw_recv_byte(uint8_t *out_data, uint8_t *out_flags)
{
    uint8_t status = spw_read(SPW_REG_STATUS);
    if (!(status & SPW_STATUS_RX_AVAIL))
        return false;
    *out_data  = spw_read(SPW_REG_RX_DATA);
    *out_flags = spw_read(SPW_REG_RX_CTRL);
    return true;
}

/**
 * spw_recv_packet() — Receive bytes until EOP/EEP into [buf].
 * [buf]      : destination buffer.
 * [max_len]  : maximum bytes to receive (not counting EOP/EEP).
 * [out_len]  : actual bytes written to buf.
 * [out_eep]  : set to true if packet ended with EEP.
 * Returns false on timeout / overflow.
 */
static inline bool spw_recv_packet(uint8_t *buf, uint16_t max_len,
                                   uint16_t *out_len, bool *out_eep)
{
    *out_len = 0;
    *out_eep = false;
    for (;;) {
        /* Spin-wait for data */
        uint32_t spin = 0;
        while (!(spw_read(SPW_REG_STATUS) & SPW_STATUS_RX_AVAIL)) {
            if (++spin > 100000u) return false;   /* timeout */
        }
        uint8_t data, flags;
        spw_recv_byte(&data, &flags);
        if (flags & SPW_RX_IS_EOP)  { return true; }
        if (flags & SPW_RX_IS_EEP)  { *out_eep = true; return true; }
        if (*out_len >= max_len)     { return false; }   /* overflow */
        buf[(*out_len)++] = data;
    }
}

/** Transmit a timecode. Value must be 6 bits [5:0] + 2 flag bits [7:6]. */
static inline void spw_send_timecode(uint8_t tc)
{
    spw_write(SPW_REG_TIMECODE, tc);
    spw_write(SPW_REG_TX_CTRL, SPW_TX_TIMECODE);
}

/** Enable interrupts by mask.  IRQ_N pin goes low when any enabled source fires. */
static inline void spw_irq_enable(uint8_t mask)
{
    spw_write(SPW_REG_IRQ_EN, mask);
}

/** Read and clear (W1C) pending IRQ flags. */
static inline uint8_t spw_irq_clear(uint8_t mask)
{
    uint8_t pending = spw_read(SPW_REG_IRQ_STATUS);
    spw_write(SPW_REG_IRQ_STATUS, pending & mask);   /* clear requested bits */
    return pending;
}

#endif /* SPW_SPI_DRIVER_H */
