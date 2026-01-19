DEVICE = GW2AR-LV18QN88C8/I7
FAMILY = GW2A-18C
TOP_MODULE = uart_test
VERILOG_FILES = ./uart_tx.v ./uart_rx.v ./uart_top.v
PROJECT = uart_verilog
CONSTRAINT_FILE = ../tangnano20k.cst

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	gowin_pack -d $(DEVICE) -o $@ $<

$(PROJECT)_pnr.json : $(PROJECT).json
	~/Tools/nextpnr/build/nextpnr-himbaechel -r --json $< --write $@ --freq 27 \
        --vopt family=$(FAMILY) \
	--vopt cst=$(CONSTRAINT_FILE) \
	--device $(DEVICE)

$(PROJECT).json : $(VERILOG_FILES)
	yosys -q -p "read_verilog $(VERILOG_FILES); synth_gowin -top $(TOP_MODULE) -json $@"

clean:
	rm -rf *.json *.fs ./verilog_out
	dune clean

.PHONY: all clean verilog_out
