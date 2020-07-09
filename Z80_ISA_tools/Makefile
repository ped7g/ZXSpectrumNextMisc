SJASMPLUS = sjasmplus
SJASMPLUSFLAGS = --msg=war --lstlab --nologo --fullpath

.PHONY: all clean gitrelease

all: test.log test.dot test.sna

clean:
	$(RM) -f tstZ80sz test.log test.dot test.dot.lst test.sna test.sna.lst

gitrelease: test.log test.dot
	$(RM) -f tstZ80sz test.dot.lst

tstZ80sz: Z80N_instructions_size.c test_Z80sizes.c
	$(CC) $(CCFLAGS) $^ -o $@

test.log: tstZ80sz
	./tstZ80sz > test.log

test.dot: test_dot.asm Z80N_instructions_size.asm Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) $< --lst="test.dot.lst" --raw=$@

test.sna: test_dot.asm Z80N_instructions_size.asm Makefile
	$(SJASMPLUS) $(SJASMPLUSFLAGS) -Dsnafile --lst="test.sna.lst" $<
