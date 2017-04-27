
GUI_MAIN_FILE=GuiMain.hs
CL_MAIN_FILE=CommandLineMain.hs
CORE_HS_FILES=Core.hs Init.hs MpvStructs.hs SrtFile.hs Loops.hs MpvFFI.hs Util.hs EventLoop.hs MpvLoops.hs
GUI_HS_FILES=Gui/GuiCore.hs
CFLAGS=-lmpv
DEBUG=-g
GHC=ghc
default: MpvLL

gui: MpvLLGui

clean:
	rm -f *.o MpvStructs.hs *.hi MpvLL

tags:   $(HS_FILES) MpvStructs.hsc
	hasktags -L .

foo.o: foo.c
	gcc $(DEBUG) -fPIC -c foo.c

MpvStructs.hs: MpvStructs.hsc
	hsc2hs MpvStructs.hsc

MpvLL: $(CORE_HS_FILES) tags foo.o $(CL_MAIN_FILE)
	$(GHC) -fPIC --make $(DEBUG) -o MpvLL -lmpv $(CL_MAIN_FILE) foo.o

MpvLLGui: $(CORE_HS_FILES) $(GUI_HS_FILES) tags foo.o $(GUI_MAIN_FILE)
	$(GHC) -threaded -fPIC --make $(DEBUG) -o MpvLLGui -lmpv $(GUI_MAIN_FILE) foo.o
