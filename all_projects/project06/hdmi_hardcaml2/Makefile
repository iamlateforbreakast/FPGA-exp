DEVICE = GW2AR-LV18QN88C8/I7
FAMILY = GW2A-18C
VERILOG_FILES = ./verilog_out/hdmi_top.v \
		./verilog_out/vesa.v \
		./verilog_out/dvi_tx.v \
		./verilog_out/dvi_encoder.v \
		./verilog_out/gowin_clkdiv.v \
		./verilog_out/gowin_elvds_obuf.v \
		./verilog_out/gowin_oser10.v \
		./verilog_out/gowin_rpll.v
SOURCES = ./lib/top.ml \
	./lib/dvi_tx.ml \
	./lib/dvi_encoder.ml \
	./lib/vesa.ml \
	./bin/generate.ml
PROJECT = hdmi_hardcaml2
CONSTRAINT_FILE = tangnano20k.cst

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	gowin_pack -d $(DEVICE) -o TBD $@

$(PROJECT)_pnr.json : $(PROJECT).json
	nextpnr-himbaechel -r --json $< --write $@ --freq 27 \
        --vopt family=$(FAMILY) \
	--vopt cst=tangnano20k.cst \
	--device $(DEVICE)

$(PROJECT).json : $(SOURCES)
	yosys -q -p "synth_gowin -json $@" $(VERILOG_FILES)

*.v: $(SOURCES)
	dune exec ./bin/generate.exe

clean:
	rm -f *.json *.fs ./verilog_out

.PHONY: all clean
