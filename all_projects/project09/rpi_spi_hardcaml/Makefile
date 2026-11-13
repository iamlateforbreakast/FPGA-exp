# Select target board with: make BOARD=nano20k (default) or make BOARD=nano4k
BOARD ?= nano20k
# Select the SPI slave with: make SPI=sclk (default) or make SPI=oversampled
SPI ?= sclk

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

ifeq ($(SPI),sclk)
SPI_SUFFIX =
else ifeq ($(SPI),oversampled)
SPI_SUFFIX = _oversampled
else
$(error Unknown SPI '$(SPI)'; expected sclk or oversampled)
endif

TOP_MODULE = top_level
VERILOG_DIR = verilog_out/$(BOARD)/$(SPI)
VERILOG_FILES = $(VERILOG_DIR)/top_level.v $(VERILOG_DIR)/spi_slave.v
SOURCES = ./lib/config_intf.ml ./lib/config.ml ./lib/spi_slave_intf.ml \
          ./lib/spi_slave_sclk.ml ./lib/spi_slave_oversampled.ml ./lib/top.ml \
          ./bin/generate.ml
PROJECT = rpi_spi_hardcaml_$(BOARD)$(SPI_SUFFIX)
ifeq ($(BOARD),nano4k)
SIM_DEFINES = -DNANO4K
endif

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	gowin_pack -d $(FAMILY) -o $@ $<

$(PROJECT)_pnr.json: $(PROJECT).json
	nextpnr-himbaechel -r --json $< --write $@ --freq 27 \
	$(FAMILY_OPT) \
	--vopt cst=$(CONSTRAINT_FILE) \
	--device $(DEVICE)

$(PROJECT).json: $(VERILOG_DIR)/$(TOP_MODULE).v
	yosys -q -p "read_verilog $(VERILOG_FILES); synth_gowin -top $(TOP_MODULE) -json $@"

$(VERILOG_DIR)/$(TOP_MODULE).v: $(SOURCES)
	BOARD=$(BOARD) SPI=$(SPI) dune exec ./bin/generate.exe

# Simulate the generated Verilog with Icarus Verilog (see sim/tb_top.v).
# SIMFLAGS=+vcd also writes tb_top.vcd.
sim: $(VERILOG_DIR)/$(TOP_MODULE).v sim/tb_top.v
	iverilog -g2012 $(SIM_DEFINES) -o $(PROJECT)_tb.vvp sim/tb_top.v $(VERILOG_FILES)
	vvp -n $(PROJECT)_tb.vvp $(SIMFLAGS)

clean:
	rm -rf *.json *.fs *.vvp *.vcd ./verilog_out
	dune clean

.PHONY: all sim clean verilog_out
