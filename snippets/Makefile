SJASMPLUS := sjasmplus
SJASMPLUSFLAGS := --msg=war --lstlab --nologo --fullpath
INCLUDES := $(wildcard *.i.asm)

.PHONY: all clean gitrelease

all: runsnippet.nex

clean:
	$(RM) runsnippet.nex runsnippet.lst

gitrelease: runsnippet.nex
	$(RM) runsnippet.lst

runsnippet.nex: runsnippet.asm $(INCLUDES) Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) $< --lst
