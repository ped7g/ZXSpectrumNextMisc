SJASMPLUS = sjasmplus
SJASMPLUSFLAGS = --msg=war --nologo --fullpath

.PHONY: all clean gitrelease

OUTPUT := show512.nex

all: $(OUTPUT)

gitrelease: $(OUTPUT)

clean:
	$(RM) $(OUTPUT) *.lst *.map

$(OUTPUT): show512.asm Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) --lst --lstlab $<
