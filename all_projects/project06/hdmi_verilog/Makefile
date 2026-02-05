DEVICE = GW2AR-LV18QN88C8/I7
FAMILY = GW2A-18C
TOP_MODULE = video_top
VERILOG_FILES = ./dvi_tx.v \
                ./dvi_encoder.v \
                ./key_led_ctrl.v \
                ./testpattern.v \
                ./video_top.v \
								./TMDS_rPLL.v
PROJECT = hdmi_verilog
CONSTRAINT_FILE = ../tangnano20k.cst

all: $(PROJECT).fs

$(PROJECT).fs: $(PROJECT)_pnr.json
	gowin_pack -d $(DEVICE) -o $@ $<

$(PROJECT)_pnr.json : $(PROJECT).json $(CONSTRAINT_FILE)
	~/Tools/nextpnr/build/nextpnr-himbaechel -r --json $< --write $@ --freq 27 \
        --vopt family=$(FAMILY) \
	--vopt cst=$(CONSTRAINT_FILE) \
	--device $(DEVICE)

$(PROJECT).json : $(TOP_MODULE).v
	yosys -q -p "read_verilog $(VERILOG_FILES); synth_gowin -top $(TOP_MODULE) -json $@"

clean:
	rm -rf *.json *.fs ./verilog_out
	dune clean

.PHONY: all clean verilog_out
