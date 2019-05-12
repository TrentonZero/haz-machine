#!/bin/bash
rm -f haz-machine.cabal 
stack clean
stack test
