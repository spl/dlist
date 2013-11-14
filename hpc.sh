ghc -fhpc -threaded -with-rtsopts=-N --make -O2 -itests tests/Main.hs -o test $*
