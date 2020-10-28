SJASMPLUS = sjasmplus
SJASMPLUSFLAGS = --msg=war --nologo
INCLUDE_FILES = $(wildcard *.i.asm)

# try to assemble the "runtime" file as standalone file (as if included elsewhere)
# then create the binary of the dot command
DISPLAYEDGE: displayedge.asm $(INCLUDE_FILES) Makefile
	$(SJASMPLUS) --zxnext $(SJASMPLUSFLAGS) --lst --lstlab displayedge.asm

DISPLAYEDGE.SNA: displayedge.asm $(INCLUDE_FILES) Makefile
	$(SJASMPLUS) --zxnext=cspect -DTESTING $(SJASMPLUSFLAGS) displayedge.asm

displayedge_rt.bin: displayedge_rt.i.asm Makefile
	$(SJASMPLUS) --msg=war --nologo -DDISPLAYEDGE_ORG_ADR=0x8000 --exp=displayedge_rt.exp displayedge_rt.i.asm

.PHONY: all cspectTest clean gitrelease fullImage

all: DISPLAYEDGE DISPLAYEDGE.SNA displayedge_rt.bin

cspectTest: DISPLAYEDGE.SNA

clean:
	$(RM) -f DISPLAYEDGE DISPLAYEDGE.SNA displayedge.lst displayedge_rt.exp displayedge_rt.bin

gitrelease: SJASMPLUSFLAGS = --msg=war --nologo
gitrelease: DISPLAYEDGE

fullImage: all
	hdfmonkey put tbblue.img DISPLAYEDGE DOT
	# hdfmonkey put tbblue.img test.cfg sys/displayedge.cfg
	mmcCSpect tbblue.img
