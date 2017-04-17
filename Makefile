
MAIN_FILE=Main.hs
HS_FILES=Main.hs MpvStructs.hs SrtFile.hs
CFLAGS=-lmpv
DEBUG=-g
GHC=ghc
default: MpvLL

clean:
	rm -f *.o MpvStructs.hs MpvLL

# foo.o: foo.c
# 	gcc $(DEBUG) -c foo.c

MpvStructs.hs: MpvStructs.hsc
	hsc2hs MpvStructs.hsc

MpvLL: $(HS_FILES)
	$(GHC) --make $(DEBUG) -o MpvLL -lmpv $(MAIN_FILE)
