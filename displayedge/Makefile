SJASMPLUS = sjasmplus
SJASMPLUSFLAGS = --msg=war --lst --lstlab --nologo
INCLUDE_FILES = constants.i.asm tilemap_font_8x6.i.asm

DISPLAYEDGE: displayedge.asm $(INCLUDE_FILES)
	$(SJASMPLUS) --zxnext $(SJASMPLUSFLAGS) displayedge.asm

DISPLAYEDGE.SNA: displayedge.asm $(INCLUDE_FILES)
	$(SJASMPLUS) --zxnext=cspect -DTESTING $(SJASMPLUSFLAGS) displayedge.asm

.PHONY: all cspectTest clean gitrelease

all: DISPLAYEDGE DISPLAYEDGE.SNA

cspectTest: DISPLAYEDGE.SNA

clean:
	$(RM) -f DISPLAYEDGE DISPLAYEDGE.SNA displayedge.lst

gitrelease: SJASMPLUSFLAGS = --msg=war --nologo
gitrelease: DISPLAYEDGE
