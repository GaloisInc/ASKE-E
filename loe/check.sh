#! /usr/bin/env sh

source ./samples.cfg
for pgm in $PROGRAMS; do
	for ext in $EXTENSIONS; do
		[ -f "$PROGRAMS_DIR/$pgm.$ext" ] || continue
		echo
		echo ";;; $pgm.$ext"
		echo
		./bounds/alpha.sh -c "$PROGRAMS_DIR/$pgm.$ext" "$PROGRAMS_DIR/symbols/$pgm.$ext.symbols"
	done
done
