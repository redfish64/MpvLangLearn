
MAIN_FILE=Main.hs
HS_FILES=Main.hs Init.hs MpvStructs.hs SrtFile.hs Loops.hs MpvFFI.hs Util.hs EventLoop.hs MpvLoops.hs
CFLAGS=-lmpv
DEBUG=-g
GHC=ghc
default: MpvLL

clean:
	rm -f *.o MpvStructs.hs *.hi MpvLL

tags:   $(HS_FILES) MpvStructs.hsc
	hasktags -L .

foo.o: foo.c
	gcc $(DEBUG) -fPIC -c foo.c

MpvStructs.hs: MpvStructs.hsc
	hsc2hs MpvStructs.hsc

MpvLL: $(HS_FILES) tags foo.o
	$(GHC) -fPIC --make $(DEBUG) -o MpvLL -lmpv $(MAIN_FILE) foo.o
