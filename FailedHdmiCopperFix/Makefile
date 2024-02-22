SJASMPLUS ?= sjasmplus
SJASMPLUSFLAGS ?= --msg=war --nologo --fullpath --lstlab=sort
MMC_IMAGE_FILE ?= ~/zx/core/10.mmc

.PHONY: all clean gitrelease test_hdmit

BIN_DIR := bin
OUT_HDMIT := $(BIN_DIR)/hdmit.dot
OUTS_LIST := $(OUT_HDMIT)

all: $(OUTS_LIST)

gitrelease: $(OUTS_LIST)

clean:
	$(RM) $(OUTS_LIST) *.lst *.map

test_hdmit: $(OUT_HDMIT)
	-hdfmonkey mkdir $(MMC_IMAGE_FILE) ped && hdfmonkey mkdir $(MMC_IMAGE_FILE) ped/d
	hdfmonkey put $(MMC_IMAGE_FILE) $(OUT_HDMIT) ped/d/
	mmcCSpect $(MMC_IMAGE_FILE) -map=hdmit.map

$(OUT_HDMIT): hdmit.asm Makefile
	@mkdir -p $(@D)
	$(SJASMPLUS) $(SJASMPLUSFLAGS) --lst=$(basename $<).lst $< --raw=$(OUT_HDMIT)
