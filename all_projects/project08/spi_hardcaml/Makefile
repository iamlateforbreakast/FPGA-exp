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

# setattr fsm_encoding=none is required, not cosmetic. The design's power-on
# reset (lib/top.ml) relies on flops powering up to zero being the INIT state,
# which holds under binary encoding but not under one-hot, where all-zeros
# decodes to no state at all and the machine never starts. yosys's FSM_RECODE
# picks one-hot for this FSM whenever it is free to, so the encoding is pinned
# here. Without it the design freezes on the Nano 4K at power-up.
# synth_gowin has no -nofsm flag, hence the attribute.
$(PROJECT).json : $(VERILOG_DIR)/$(TOP_MODULE).v
	yosys -q -p "read_verilog $(VERILOG_FILES); setattr -set fsm_encoding \"none\" w:*; synth_gowin -top $(TOP_MODULE) -json $@"

$(VERILOG_DIR)/$(TOP_MODULE).v: $(SOURCES)
	BOARD=$(BOARD) dune exec ./bin/generate.exe

clean:
	rm -rf *.json *.fs ./verilog_out
	dune clean

.PHONY: all clean verilog_out
