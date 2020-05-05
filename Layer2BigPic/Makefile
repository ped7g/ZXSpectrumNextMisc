SJASMPLUS = sjasmplus
SJASMPLUSFLAGS = --msg=war --nologo --fullpath

.PHONY: all clean gitrelease

OUTPUT := bigpic.nex
INTERMEDIATE_FILES := bigpic_top.tga bigpic_bot.tga

all: $(OUTPUT)

gitrelease: $(OUTPUT)

clean:
	$(RM) $(OUTPUT) *.tga *.lst *.map

bigpic_top.tga: bigpic.png Makefile
	convert $< -rotate 270 -crop 256x640+0+0 tga:$@

bigpic_bot.tga: bigpic.png Makefile
	convert $< -rotate 270 -crop 256x640+256+0 tga:$@

$(OUTPUT): bigpic.asm Makefile $(INTERMEDIATE_FILES)
	$(SJASMPLUS) $(SJASMPLUSFLAGS) --lst="$(basename $<).lst" --lstlab $<
