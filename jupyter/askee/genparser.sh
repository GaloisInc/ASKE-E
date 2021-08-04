#!/bin/sh
rm -rf antlrgen
antlr -Dlanguage=Python3 -o antlrgen -visitor ASKEECommand.g4