.PHONY: all cspectTest clean gitrelease

all: test.log

tstZ80sz: classicZ80_instructions_size.c test_Z80sizes.c
	$(CC) $(CCFLAGS) $^ -o $@

test.log: tstZ80sz
	./tstZ80sz > test.log

clean:
	$(RM) -f tstZ80sz test.log

gitrelease: test.log
	$(RM) -f tstZ80sz
