SJASMPLUS = sjasmplus
SJASMPLUSFLAGS = --msg=war --nologo --fullpath

.PHONY: all clean gitrelease

OUTPUT := A_drvHID.nex

all: $(OUTPUT)

gitrelease: $(OUTPUT)

clean:
	$(RM) $(OUTPUT) *.lst *.map

$(OUTPUT): A_drvHID.asm constants.i.asm Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) --lst="$(basename $<).lst" --lstlab $<
