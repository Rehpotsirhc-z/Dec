#!/bin/bash
stack build --arch x86_64
cp .stack-work/dist/x86_64-linux-tinfo6/ghc-9.4.8/build/dec/dec bin/dec-0.2.0.0-x86_64
