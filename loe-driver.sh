#! /usr/bin/env sh

### Test harness to exercise LOE calculations.

HERE=$(cd `dirname $0`; pwd)
OPTS=

[ $# -ge 2 ] || {
	echo "usage: $0 [-t|--tokens] FILE_OR_DIR FILE_OR_DIR..."
	exit 1
}

[ "$1" = -t -o "$1" = --tokens ] && { OPTS=$1; shift; }
function remark { echo "$@"; }
function gather { gf=`mktemp`; find "$*" -type f -exec cat \{\} \; > $gf; echo $gf; }
function cmp { $HERE/loe-metrics.py $OPTS "$1" "$2"; }
function cmpderiv {
	remark Derivations:
	for f in "$@"; do
		remark "  $f"
	done
	while test $# -ge 2; do
		f1="$1"
		remark == Basis: $f1
		step=0
		for f2 in "$@"; do
			remark -- $step step
			step=$(($step+1))
			test -d "$f1" && {
				f1="$(gather $f1)"
				clean1=$f1
				remark gather: $1 is $f1
			}
			test -d "$f2" && {
				f2="$(gather $f2)"
				clean2=$f2
				remark gather: $2 is $f2
			}
			cmp "$f1" "$f2"
		done
		shift
	done
	[ -n "$clean1" ] && rm -f $clean1
	[ -n "$clean2" ] && rm -f $clean2
}

cmpderiv "$@"
