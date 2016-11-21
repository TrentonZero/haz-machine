#!/bin/bash
while (true) do
#cabal clean configure build --enable-tests &> /dev/null
date &> build.log
stack test >> build.log 2>&1
clear
cat build.log | ./hilite.sh -f green PASS | ./hilite.sh -f red FAIL | ./hilite.sh -f green Pass | ./hilite.sh -f red Fail | ./hilite.sh -f red error
read -t 1 -p "Press Ctrl-C to break"
done
