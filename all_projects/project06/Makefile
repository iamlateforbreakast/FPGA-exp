PROJ = running_light
PIN_DEF = $(PROJ).cst
DEVICE = GW1NR-LV9QN88PC6/I5
DEVICE_FAMILY = GW1N-9C
# Verilog file (same as projectname)
SOURCE = $(PROJ).v
# board name for OpenFPGALoader
BOARD = tangnano9k
# where should the code reside: "SRAM" or "FLASH"
TARGET ?= SRAM

$(info PROJ = $(PROJ))   # Print variables for debugging

all: $(PROJ).fs      # final bitstream file

$(PROJ)-yosys.json: $(SOURCE)
    yosys -q -p "synth_gowin -json $@" $(SOURCE)
    $(info Building $(PROJ)-yosys.json)

$(PROJ)-nextpnr.json: $(PIN_DEF) $(PROJ)-yosys.json # Create place and route JSON file from Yosys JSON file
      nextpnr-gowin --json $(PROJ)-yosys.json --write $@ --device $(DEVICE) --family $(DEVICE_FAMILY) --cst $(PIN_DEF)

$(PROJ).fs: $(PROJ)-nextpnr.json # pack the NextPNR JSON-file to final Gowin pack-file
      PATH="$(HOME)/Programme/apycula_env/bin:$$PATH" gowin_pack -d $(DEVICE_FAMILY) -o $@ $(PROJ)-nextpnr.json

prog: $(PROJ).fs                 # load bitstream fs-file to SRAM or FLASH

    ifeq ($(TARGET),FLASH)
      openFPGALoader -f --freq 2000000 -b $(BOARD) $(PROJ).fs
    else
      openFPGALoader -m --freq 2000000 -b $(BOARD) $(PROJ).fs
    endif

clean:                           # clean the files that are needed no more
      rm -f *.json *.fs

.PHONY: all prog clean