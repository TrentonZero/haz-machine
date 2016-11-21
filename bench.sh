#!/bin/bash
date 
stack bench --benchmark-arguments="-o report.html" 
#cabal bench  --benchmark-options="-o report.html" 
