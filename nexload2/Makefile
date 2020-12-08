SJASMPLUS = sjasmplus
SJASMPLUSFLAGS = --msg=war --lst --lstlab --nologo

NEXLOAD2: nexload2.asm
	$(SJASMPLUS) --zxnext $(SJASMPLUSFLAGS) nexload2.asm

nexload2.sna: nexload2.asm nexload2.test.progress.i.asm
	$(SJASMPLUS) --zxnext=cspect -DTESTING $(SJASMPLUSFLAGS) nexload2.asm

.PHONY: all cspectTest clean gitrelease checkExamine

all: NEXLOAD2 nexload2.sna checkExamine

cspectTest: nexload2.sna

clean:
	$(RM) -f NEXLOAD2 nexload2.sna nexload2.lst

gitrelease: SJASMPLUSFLAGS = --msg=war --nologo
gitrelease: NEXLOAD2 checkExamine

checkExamine: examine.asm
	$(SJASMPLUS) --msg=war examine.asm -DNEX_FILE=\"tmHiCol.nex\"
