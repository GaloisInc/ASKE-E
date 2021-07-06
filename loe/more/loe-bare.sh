#! /usr/bin/env sh

# Compute a LOE for a given source file.

PGM=$(basename "$0")
[ $# -gt 0 ] || {
	echo "usage: $PGM SOURCEFILE"
	exit 1
}

[ -f "$1" ] || {
	echo "$PGM: $1: No such file"
	exit 1
}

[ -r "$1" ] || {
	echo "$PGM: $1: No readable file"
	exit 1
}

SRCFILE=$(basename "$1")

SRC_CMP=$(compress -cf "$SRCFILE"|wc -c)

SRC_USYM=$(cat "$SRCFILE"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)

printf "= %15s %6d %6d\n" \
	$SRCFILE $SRC_CMP $SRC_USYM
