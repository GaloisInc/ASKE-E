#! /usr/bin/env sh

PROGRAMS_DIR=samples
PROGRAMS="hello quicksort toposort ackermann ode-1 ode-1_1 ode-1_2 ode-1_3 ode-1_4 ode-1_5 ode-2 ode-2_1 ode-2_2 ode-2_3 ode-2_4 ode-2_5 ode-3 ode-3_1 ode-3_2 ode-3_3 ode-3_4 ode-3_5"
EXTENSIONS="c cpp f90 hs txt jl py"

header_filter () {
	awk 'BEGIN {hl=0} /^;/ {if (hl<1) {print; hl=hl+1}} /^=/ {print} /^$/ {print}'
}

for pgm in $PROGRAMS; do
	for ext in $EXTENSIONS; do
		[ -f "$PROGRAMS_DIR/$pgm.$ext" ] || continue
		./loe.sh "$PROGRAMS_DIR/$pgm.$ext"
	done
done | header_filter | sed 's/^\(;\|=\) //'
