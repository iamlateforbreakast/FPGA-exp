# Select target board with: make BOARD=nano20k (default) or make BOARD=nano4k
BOARD ?= nano20k

ifeq ($(BOARD),nano20k)
DEVICE = GW2AR-LV18QN88C8/I7
FAMILY = GW2A-18C
FAMILY_OPT = --vopt family=$(FAMILY)
CONSTRAINT_FILE = ../tangnano20k.cst
# True LVDS on this board, so stock gowin_pack is fine.
PACK = gowin_pack
else ifeq ($(BOARD),nano4k)
DEVICE = GW1NSR-LV4CQN48PC7/I6
FAMILY = GW1NS-4
FAMILY_OPT =
CONSTRAINT_FILE = ../tangnano4k.cst
# This board's HDMI needs ELVDS_OBUF on pins apycula marks TRUELVDS, which
# stock gowin_pack refuses to place. See ../tools/gowin_pack_elvds.py.
PACK = python3 ../tools/gowin_pack_elvds.py
else
$(error Unknown BOARD '$(BOARD)'; expected nano20k or nano4k)
endif

TOP_MODULE = top_level
VERILOG_DIR = verilog_out/$(BOARD)
VERILOG_FILES = $(VERILOG_DIR)/dvi_tx.v \
                $(VERILOG_DIR)/dvi_encoder.v \
                $(VERILOG_DIR)/leds.v \
                $(VERILOG_DIR)/test_pattern.v \
                $(VERILOG_DIR)/top_level.v
SOURCES = ./lib/top.ml \
          ./lib/dvi_tx.ml \
          ./lib/dvi_encoder.ml \
          ./lib/test_pattern.ml \
          ./lib/leds.ml \
          ./lib/config_intf.ml \
          ./bin/generate.ml
PROJECT = hdmi_hardcaml_$(BOARD)

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	$(PACK) -d $(FAMILY) -o $@ $<

# NOTE: nextpnr's -r is --randomize-seed, not "route". Leaving it in made
# every build pick a different placement, so a bitstream that worked could
# stop working on an unchanged rebuild - and the PLL->CLKDIV HCLK route
# failed outright on some seeds. Pin the seed instead so builds are
# reproducible and hardware A/B comparisons actually compare one variable.
$(PROJECT)_pnr.json : $(PROJECT).json
	nextpnr-himbaechel --seed 1 --json $< --write $@ --freq 27 \
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
