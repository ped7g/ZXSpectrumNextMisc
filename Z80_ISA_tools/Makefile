SJASMPLUS = sjasmplus
SJASMPLUSFLAGS = --msg=war --lstlab --nologo --fullpath

.PHONY: all clean gitrelease

all: test.log test.dot test.sna

clean:
	$(RM) -f tstZ80sz test.log test.dot test.dot.lst test_z80.dot test_z80.dot.lst test.sna test.sna.lst test_z80.sna test_z80.sna.lst

gitrelease: test.log test.dot test_z80.dot
	$(RM) -f tstZ80sz test.dot.lst test_z80.dot.lst

tstZ80sz: Z80N_instructions_size.c test_Z80sizes.c
	$(CC) $(CCFLAGS) $^ -o $@

test.log: tstZ80sz
	./tstZ80sz > test.log

test.dot: test_dot.asm Z80N_instructions_size.asm Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) $< --lst="test.dot.lst" --raw=$@

test_z80.dot: test_dot.asm Z80N_instructions_size.asm Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) -DCLASSIC_Z80 $< --lst="test_z80.dot.lst" --raw=$@

test.sna: test_dot.asm Z80N_instructions_size.asm Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) -Dsnafile --lst="test.sna.lst" $<

test_z80.sna: test_dot.asm Z80N_instructions_size.asm Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) -DCLASSIC_Z80 -Dsnafile --lst="test_z80.sna.lst" $<
