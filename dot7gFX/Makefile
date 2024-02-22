SJASMPLUS ?= sjasmplus
SJASMPLUSFLAGS ?= --msg=war --nologo --fullpath --lstlab=sort
MMC_IMAGE_FILE ?= ~/zx/core/10.mmc

.PHONY: all clean gitrelease test_fadeout

BIN_DIR := bin
OUT_FADEOUT := $(BIN_DIR)/7gfx_fo.dot
OUTS_LIST := $(OUT_FADEOUT)

all: $(OUTS_LIST)

gitrelease: $(OUTS_LIST)

clean:
	$(RM) $(OUTS_LIST) *.lst *.map

test_fadeout: $(OUT_FADEOUT)
	hdfmonkey get $(MMC_IMAGE_FILE) demos/bmp256converts/bitmaps/krull.bmp > krull.bmp
	-hdfmonkey mkdir $(MMC_IMAGE_FILE) ped && hdfmonkey mkdir $(MMC_IMAGE_FILE) ped/d
	hdfmonkey put $(MMC_IMAGE_FILE) krull.bmp ped/d/
	hdfmonkey put $(MMC_IMAGE_FILE) test.bas ped/d/
	hdfmonkey put $(MMC_IMAGE_FILE) $(OUT_FADEOUT) ped/d/
	mmcCSpect $(MMC_IMAGE_FILE) -map=fadeout.map

$(OUT_FADEOUT): fadeout.asm Makefile
	@mkdir -p $(@D)
	$(SJASMPLUS) $(SJASMPLUSFLAGS) --lst=$(basename $<).lst $< --raw=$(OUT_FADEOUT)
