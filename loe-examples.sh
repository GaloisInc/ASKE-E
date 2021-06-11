#! /usr/bin/env sh

### Test harness to exercise LOE calculations.

HERE=$(cd `basename $0`; pwd)

$HERE/loe-driver.sh \
	examples/latex/sir.latex \
	examples/rnet/sir.rnet \
	modelRepo/easel/sir.easel \
	modelCpp/sir.easel.cpp
