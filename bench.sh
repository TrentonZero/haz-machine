#!/bin/bash
date 
cabal bench  --benchmark-options="-o report.html" 
