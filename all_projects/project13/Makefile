# Select target board with: make BOARD=nano20k (default) or make BOARD=nano4k
BOARD ?= nano20k

ifeq ($(BOARD),nano20k)
DEVICE = GW2AR-LV18QN88C8/I7
FAMILY = GW2A-18C
FAMILY_OPT = --vopt family=$(FAMILY)
CONSTRAINT_FILE = tangnano20k.cst
VERILOG_FILES = verilog_out/$(BOARD)/top_level.v \
                verilog_out/$(BOARD)/sdram_system.v \
                verilog_out/$(BOARD)/sdram.v \
                verilog_out/$(BOARD)/memory_test.v \
                verilog_out/$(BOARD)/pll.v
else ifeq ($(BOARD),nano4k)
DEVICE = GW1NSR-LV4CQN48PC7/I6
FAMILY = GW1NS-4
FAMILY_OPT =
CONSTRAINT_FILE = tangnano4k.cst
VERILOG_FILES = verilog_out/$(BOARD)/top_level.v \
                verilog_out/$(BOARD)/hyperram_system.v \
                verilog_out/$(BOARD)/hyperram.v \
                verilog_out/$(BOARD)/debug_spi.v \
                verilog_out/$(BOARD)/memory_test.v \
                verilog_out/$(BOARD)/pll.v
else
$(error Unknown BOARD '$(BOARD)'; expected nano20k or nano4k)
endif

TOP_MODULE = top_level
VERILOG_DIR = verilog_out/$(BOARD)
PROJECT = memory_hardcaml_$(BOARD)
SOURCES = $(wildcard lib/*.ml) bin/generate.ml

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	gowin_pack -d $(FAMILY) -o $@ $<

$(PROJECT)_pnr.json: $(PROJECT).json
	nextpnr-himbaechel --json $< --write $@ --freq 27 --seed 1 \
        $(FAMILY_OPT) \
	--vopt cst=$(CONSTRAINT_FILE) \
	--device $(DEVICE)

$(PROJECT).json: $(VERILOG_FILES)
	yosys -q -p "read_verilog $(VERILOG_FILES); synth_gowin -top $(TOP_MODULE) -json $@"

$(VERILOG_DIR)/$(TOP_MODULE).v: $(SOURCES)
	BOARD=$(BOARD) dune exec ./bin/generate.exe

clean:
	rm -rf *.json *.fs ./verilog_out
	dune clean

.PHONY: all clean verilog_out
