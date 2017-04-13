

GHC=ghc
CFLAGS=-lmpv
DEPS =
OBJS=MpvLL.o MpvStructs.o Main.o
COBJS=foo.o
DEBUG=-g

default: MpvLL

clean:
	rm *.o MpvStructs.hs MpvLL

foo.o: foo.c
	gcc $(DEBUG) -c foo.c

%.o: %.hs 
	$(GHC) $(DEBUG) -c -o $@ $< $(CFLAGS)

MpvStructs.hs: MpvStructs.hsc
	hsc2hs MpvStructs.hsc

MpvLL: $(OBJS) $(COBJS)
	$(GHC) $(DEBUG) -o MpvLL -lmpv $(OBJS) $(COBJS)
