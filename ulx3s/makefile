IDCODE ?= 0x41113043 # 85f

all: prog

TOP_ENTITY_FILE=../src/$(TOP_ENTITY_MODULE).hs
VERILOG_DIR=./verilog/$(TOP_ENTITY_MODULE).topEntity
TOP_VERILOG_FILE=$(VERILOG_DIR)/top.v

$(TOP_VERILOG_FILE): $(TOP_ENTITY_FILE)
	$(info generating [${TOP_VERILOG_FILE}])
	stack run clash -- \
	$(TOP_ENTITY_FILE) \
	-i../src \
	--verilog \
	-odir=odir/ \
	-hidir=hidir/


$(TOP_ENTITY_MODULE).json: $(TOP_VERILOG_FILE)
	yosys \
		-p "synth_ecp5 -top top -json $@" \
		-E $(TOP_ENTITY_MODULE).d \
		$<

$(TOP_ENTITY_MODULE).config: $(TOP_ENTITY_MODULE).json
	nextpnr-ecp5 \
		--json $< \
		--textcfg $@ \
		--lpf ulx3s_v20.lpf \
		--85k \
		--package CABGA381

$(TOP_ENTITY_MODULE).bit: $(TOP_ENTITY_MODULE).config
	ecppack --idcode $(IDCODE) $< $@

.PHONY: prog
prog: $(TOP_ENTITY_MODULE).bit
	openFPGALoader -f -b ulx3s $<

.PHONY: clean
clean:
	rm -rf *.json *.config *.bit *.d verilog vout odir hdir