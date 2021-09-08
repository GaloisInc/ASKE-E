#! /usr/bin/env sh

source ./samples.cfg
header_filter () {
	awk 'BEGIN {hl=0} /^;/ {if (hl<1) {print; hl=hl+1}} /^=/ {print} /^$/ {print}'
}

for pgm in $PROGRAMS; do
	for ext in $EXTENSIONS; do
		[ -f "$PROGRAMS_DIR/$pgm.$ext" ] || continue
		./loe.sh "$PROGRAMS_DIR/$pgm.$ext"
	done
done | header_filter | sed 's/^\(;\|=\) //'
