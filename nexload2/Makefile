SJASMPLUSFLAGS=--msg=war --lst --lstlab --nologo

NEXLOAD2: nexload2.asm
	sjasmplus --zxnext $(SJASMPLUSFLAGS) nexload2.asm

nexload2.sna: nexload2.asm nexload2.test.progress.i.asm
	sjasmplus --zxnext=cspect -DTESTING $(SJASMPLUSFLAGS) nexload2.asm

.PHONY: all cspectTest clean

all: NEXLOAD2 nexload2.sna

cspectTest: nexload2.sna

clean:
	rm -f NEXLOAD2 nexload2.sna nexload2.lst
