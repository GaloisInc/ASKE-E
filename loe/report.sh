#! /usr/bin/env sh

PROGRAMS_DIR=samples
PROGRAMS="hello quicksort toposort ackermann"
EXTENSIONS="c cpp f90 hs"

header_filter () {
	awk 'BEGIN {hl=0} /^;/ {if (hl<2) {print; hl=hl+1}} /^=/ {print} /^$/ {print}'
}

for pgm in $PROGRAMS; do
	for ext in $EXTENSIONS; do
		./loe.sh "$PROGRAMS_DIR/$pgm.$ext"
	done | header_filter
	echo
done | sed 's/^\(;\|=\) //'
