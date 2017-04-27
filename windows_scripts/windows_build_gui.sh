#execute from main directory in cygwin using: bash ./windows_crap/windows_build.sh
#dependencies are haskell platform, cabal libraries specified in MpvLL.cabal, wxhaskell, and libmpv
rm MpvLLGui.exe *.o Gui/*.o
hsc2hs.exe -I 'd:/libmpv/include' MpvStructs.hsc
gcc -I d:/libmpv/include -c foo.c
ghc -static -fPIC --make -o MpvLL -L"d:/libmpv/64/;D:/wxInstall-Achelanne-64-0.1/DLLs;D:/wxInstall-Achelanne-64-0.1/wxWidgets/lib/gcc_dll" d:/libmpv/64//libmpv.dll.a GuiMain.hs foo.o

  
