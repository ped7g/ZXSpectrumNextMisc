SJASMPLUS = sjasmplus
SJASMPLUSFLAGS = --msg=war --nologo --fullpath

.PHONY: all clean gitrelease

OUTPUT := fadeout.nex
INTERMEDIATE_FILES := fadeout.tga

all: $(OUTPUT)

gitrelease: $(OUTPUT)

clean:
	$(RM) $(OUTPUT) *.tga *.lst *.map

fadeout.tga: fadeout.png Makefile
	convert $< -rotate 270 tga:$@

$(OUTPUT): fadeout.asm Makefile $(INTERMEDIATE_FILES)
	$(SJASMPLUS) $(SJASMPLUSFLAGS) --lst="$(basename $<).lst" --lstlab $<
