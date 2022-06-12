SJASMPLUS ?= sjasmplus
SJASMPLUSFLAGS ?= --msg=war --nologo --fullpath --lstlab=sort
MMC_IMAGE_FILE ?= ~/zx/core/mmc-1.3.2-128mb/10.mmc

.PHONY: all clean gitrelease test

BIN_DIR := bin
OUTPUT := $(BIN_DIR)/7gfx_fo.dot

all: $(OUTPUT)

gitrelease: $(OUTPUT)

clean:
	$(RM) $(OUTPUT) *.lst *.map

test: $(OUTPUT)
	hdfmonkey get $(MMC_IMAGE_FILE) demos/bmp256converts/bitmaps/krull.bmp > krull.bmp
	-hdfmonkey mkdir $(MMC_IMAGE_FILE) ped && hdfmonkey mkdir $(MMC_IMAGE_FILE) ped/d
	hdfmonkey put $(MMC_IMAGE_FILE) krull.bmp ped/d/
	hdfmonkey put $(MMC_IMAGE_FILE) test.bas ped/d/
	hdfmonkey put $(MMC_IMAGE_FILE) $(OUTPUT) ped/d/
	mmcCSpect $(MMC_IMAGE_FILE) -map=fadeout.map

$(OUTPUT): fadeout.asm Makefile
	@mkdir -p $(@D)
	$(SJASMPLUS) $(SJASMPLUSFLAGS) --lst=$(basename $<).lst $< --raw=$(OUTPUT)
