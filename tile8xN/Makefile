SJASMPLUS := sjasmplus
SJASMPLUSFLAGS := --msg=war --lstlab --nologo --fullpath
INCLUDES := $(wildcard *.i.asm)
EXAMPLE_INC := $(wildcard example/*.i.asm)
DSPEDGE_RT_SRC := "../displayedge/displayedge_rt.i.asm"

.PHONY: all clean gitrelease

# TODO "all" currently builds only the example NEX file

all: test8xN.nex

clean:
	$(RM) test8xN.nex test8xN.lst test8xN.map

gitrelease: Makefile test8xN.nex
	$(RM) test8xN.lst test8xN.map

test8xN.nex: example/test8xN.asm $(EXAMPLE_INC) $(INCLUDES) Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) --lst=test8xN.lst $<

# TODO use probably similar mechanics like displayedge_rt to enable binary-library build, with export file
