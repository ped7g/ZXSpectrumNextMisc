SJASMPLUS := sjasmplus
SJASMPLUSFLAGS := --msg=war --lstlab --nologo --fullpath
INCLUDES := $(wildcard *.i.asm)

.PHONY: all clean gitrelease

all: test.nex

clean:
	$(RM) -f test.nex test.lst

gitrelease: test.nex
	$(RM) -f test.lst

test.nex: test.asm $(INCLUDES) Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) $< --lst
