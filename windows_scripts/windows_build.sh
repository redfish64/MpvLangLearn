#execute from main directory in cygwin using: bash ./windows_crap/windows_build.sh
#dependencies are haskell platform, cabal libraries specified in MpvLL.cabal, and libmpv
rm MpvLL.exe *.o
hsc2hs.exe -I 'd:/libmpv/include' MpvStructs.hsc
gcc -I d:/libmpv/include -c foo.c
ghc -static -fPIC --make -o MpvLL -L"d:/libmpv/64/" d:/libmpv/64//libmpv.dll.a Main.hs foo.o
