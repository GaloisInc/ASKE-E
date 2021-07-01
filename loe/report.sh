#! /usr/bin/env sh

PROGRAMS="hello quicksort"
EXTENSIONS="c cpp f90 hs"

header_filter () {
	awk 'BEGIN {hl=0} /^;/ {if (hl<2) {print; hl=hl+1}} /^=/ {print} /^$/ {print}'
}

for pgm in $PROGRAMS; do
	for ext in $EXTENSIONS; do
		./loe.sh "$pgm.$ext"
	done | header_filter
	echo
done | sed 's/^\(;\|=\) //'
