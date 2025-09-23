DEVICE = GW2AR-LV18QN88C8/I7
FAMILY = GW2A-18C
TOP_MODULE = screen
VERILOG_FILES = ./verilog_out/screen.v
SOURCES = ./lib/screen.ml ./bin/generate.ml
PROJECT = spi_hardcaml
CONSTRAINT_FILE = tangnano20k.cst

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	gowin_pack -d $(DEVICE) -o $@ $<

$(PROJECT)_pnr.json : $(PROJECT).json
	nextpnr-himbaechel -r --json $< --write $@ --freq 27 \
        --vopt family=$(FAMILY) \
	--vopt cst=tangnano20k.cst \
	--device $(DEVICE)

$(PROJECT).json : verilog_out
	yosys -q -p "read_verilog $(VERILOG_FILES); synth_gowin -top $(TOP_MODULE) -json $@"

verilog_out: $(SOURCES)
	dune exec ./bin/generate.exe

clean:
	rm -f *.json *.fs ./verilog_out
	dune clean

.PHONY: all clean verilog_out
