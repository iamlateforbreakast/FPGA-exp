# Select target board with: make BOARD=nano20k (default) or make BOARD=nano4k
BOARD ?= nano20k

ifeq ($(BOARD),nano20k)
DEVICE = GW2AR-LV18QN88C8/I7
FAMILY = GW2A-18C
FAMILY_OPT = --vopt family=$(FAMILY)
CONSTRAINT_FILE = ../tangnano20k.cst
else ifeq ($(BOARD),nano4k)
DEVICE = GW1NSR-LV4CQN48PC7/I6
FAMILY = GW1NS-4
FAMILY_OPT =
CONSTRAINT_FILE = ../tangnano4k.cst
else
$(error Unknown BOARD '$(BOARD)'; expected nano20k or nano4k)
endif

TOP_MODULE = uart_test
VERILOG_FILES = ./uart_tx.v ./uart_rx.v ./uart_top.v
PROJECT = uart_verilog_$(BOARD)

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	gowin_pack -d $(FAMILY) -o $@ $<

$(PROJECT)_pnr.json : $(PROJECT).json
	~/Tools/nextpnr/build/nextpnr-himbaechel -r --json $< --write $@ --freq 27 \
        $(FAMILY_OPT) \
	--vopt cst=$(CONSTRAINT_FILE) \
	--device $(DEVICE)

$(PROJECT).json : $(VERILOG_FILES)
	yosys -q -p "read_verilog $(VERILOG_FILES); synth_gowin -top $(TOP_MODULE) -json $@"

clean:
	rm -rf *.json *.fs ./verilog_out
	dune clean

.PHONY: all clean verilog_out
