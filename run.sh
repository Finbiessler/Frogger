#!/bin/bash
stack ghc -- --make Main.hs
echo "Starting Main..."
./Main
