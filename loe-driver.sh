#! /usr/bin/env sh

### Test harness to exercise LOE calculations.

HERE=$(cd `basename $0`; pwd)
OPTS=

function remark { echo "$@"; }
function gather { gf=`mktemp`; find $1 -type f -exec cat \{\} \; > $gf; echo $gf; }
function cmp { $HERE/loe-metrics.py $OPTS $1 $2; }
function cmpderiv {
	remark Derivations:
	for f in $*; do
		remark "  $f"
	done
	while test $# -ge 2; do
		f1=$1
		remark == Basis: $f1
		step=0
		for f2 in $*; do
			remark -- $step step
			step=$(($step+1))
			cmp $f1 $f2
		done
		shift
	done
}

cmpderiv "$@"
