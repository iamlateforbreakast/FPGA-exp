DEVICE = GW2AR-LV18QN88C8/I7
FAMILY = GW2A-18C
TOP_MODULE = top_level
VERILOG_FILES = ./verilog_out/leds.v ./verilog_out/top_level.v
SOURCES = ./lib/top.ml ./lib/leds.ml ./bin/generate.ml
PROJECT = leds_hardcaml
CONSTRAINT_FILE = ../tangnano20k.cst

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	gowin_pack -d $(DEVICE) -o $@ $<

$(PROJECT)_pnr.json : $(PROJECT).json
	~/Tools/nextpnr/build/nextpnr-himbaechel -r --json $< --write $@ --freq 27 \
        --vopt family=$(FAMILY) \
	--vopt cst=$(CONSTRAINT_FILE) \
	--device $(DEVICE)

$(PROJECT).json : verilog_out/$(TOP_MODULE).v
	yosys -q -p "read_verilog $(VERILOG_FILES); synth_gowin -top $(TOP_MODULE) -json $@"

verilog_out/$(TOP_MODULE).v: $(SOURCES)
	dune exec ./bin/generate.exe

clean:
	rm -rf *.json *.fs ./verilog_out
	dune clean

.PHONY: all clean verilog_out
