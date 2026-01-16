# Would it be possible to synthesize the verilog spacewire core on the tang nano 20k?

--------------

To connect the FreeCores SpaceWire IP to the Sipeed Tang Nano 20K using LVDS, you must map the design's differential data and strobe signals to specific FPGA pin pairs that support differential I/O.
1. Identify LVDS Pin Pairs
The GW2AR-18 FPGA on the Tang Nano 20K uses specific pin pairs for differential signals (LVDS). 
Differential Pairs: You must select adjacent pins that form a hardware-defined differential pair (e.g., IOT29A as positive and IOT29B as negative).
Confirmed LVDS Pins: Pins 29/30 and 74/75 are known pairs often used for differential I/O on this board.
Voltage Bank Constraint: On the Tang Nano 20K, LVDS banks are typically powered at 3.3V. If you configure a pin pair as LVDS, you cannot use other pins in that same bank as standard single-ended 3.3V CMOS pins. 
2. Configure I/O in Gowin IDE
You cannot simply assign pins in Verilog; you must use the FloorPlanner or a .cst (constraint) file to define the electrical characteristics. 
IO_TYPE: Set the pin type to LVDS25 or LVDS33 in the constraint file.
Terminations: Some banks on the Nano 20K do not support internal 100-ohm differential resistors; you may need to add an external 100-ohm resistor across the positive and negative pins for signal integrity. 
3. SpaceWire Physical Mapping
SpaceWire uses two differential pairs per direction: Data (D) and Strobe (S). To connect a single SpaceWire link, you need 4 differential pairs (8 pins total): 
Transmit:
tx_data_p / tx_data_n (Pair 1)
tx_strobe_p / tx_strobe_n (Pair 2)
Receive:
rx_data_p / rx_data_n (Pair 3)
rx_strobe_p / rx_strobe_n (Pair 4)
4. Implementation Steps
Instantiate Buffers: In your top-level Verilog file, wrap the SpaceWire IP signals with Gowin-specific differential primitives, such as TLVDS_OBUF for outputs and TLVDS_IBUF for inputs.
Define Constraints: Create a .cst file in the Gowin FPGA Designer IDE to bind your ports to the physical pin numbers (e.g., IO_LOC "tx_data_p" 29;).
Check Schematic: Always verify the latest Tang Nano 20K Schematic to ensure the pins you choose are not hard-wired to other onboard peripherals like the HDMI or SD card slots.

----------------

In the FreeCores SpaceWire repository, there isn't a single "top level" file; instead, the top-level module depends on whether you are synthesizing a standalone channel, a node interface, or a full router. 
According to the project's documentation, you should use one of the following as your top module:
SPW_CODEC.v: Use this if you want a single SpaceWire channel (CODEC). This is the most common starting point for integrating a SpaceWire interface into your own custom design.
SPW_I_vlogcore.v: Use this for a node device interface. It integrates three CODECs with glue logic and a Wishbone interface (COMI/HOCI) to connect the SpaceWire network to a host.
SWR_vlogcore.v: Use this for a complete SpaceWire Router. It integrates multiple interfaces and a routing matrix (SwitchCore.v). 
Integration Notes for Tang Nano 20K
LVDS Primitives: The original code uses Xilinx-specific primitives for LVDS drivers and receivers. Since you are targeting the Tang Nano 20K (Gowin FPGA), you must replace these with Gowin equivalents, such as TLVDS_IBUF and TLVDS_OBUF.
FIFOs: The design relies on FIFO components. While it includes a synchronous FIFO for the transmitter, the receiver uses a "plesiochronous FIFO". You may need to regenerate these using the Gowin IP Core Generator to ensure they are optimized for the Tang Nano's resources. 

