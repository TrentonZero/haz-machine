#!/run/current-system/sw/bin/bash
rm -f haz-machine.cabal 
stack clean
stack test
