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

TOP_MODULE = top_level
VERILOG_DIR = verilog_out/$(BOARD)
VERILOG_FILES = $(VERILOG_DIR)/top_level.v $(VERILOG_DIR)/screen_spi.v
SOURCES = ./lib/screen_spi.ml ./bin/generate.ml
PROJECT = spi_hardcaml_$(BOARD)

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	gowin_pack -d $(FAMILY) -o $@ $<

$(PROJECT)_pnr.json : $(PROJECT).json
	nextpnr-himbaechel -r --json $< --write $@ --freq 27 \
        $(FAMILY_OPT) \
	--vopt cst=$(CONSTRAINT_FILE) \
	--device $(DEVICE)

$(PROJECT).json : $(VERILOG_DIR)/$(TOP_MODULE).v
	yosys -q -p "read_verilog $(VERILOG_FILES); synth_gowin -top $(TOP_MODULE) -json $@"

$(VERILOG_DIR)/$(TOP_MODULE).v: $(SOURCES)
	BOARD=$(BOARD) dune exec ./bin/generate.exe

clean:
	rm -rf *.json *.fs ./verilog_out
	dune clean

.PHONY: all clean verilog_out
