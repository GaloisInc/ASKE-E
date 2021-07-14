#! /usr/bin/env sh

# Estimate the Kolmogorov complexity of a program text after doing an
# alpha-conversion on the text. The alpha-conversion helps to establish
# a tighter bound on K(s), as estimated by the compressibility of the
# source text s, by minimizing the length of non-reserved and non-library
# symbols.
#
# We do not preprocess the source text. We assume that the source text
# is comment-free.
#
# We count the number of unique symbols (excluding punctuation) in
# both the source and assembler texts as an approximation of the number of
# distinct identifiers and operators; this can be taken as an indicator
# of the number of concepts to be understood and applied by the programmer.

HERE=$(cd `dirname "$0"`; pwd)
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
SRCPATH=$(dirname "$1")

K_OF_S=$($HERE/bounds/alpha.sh "$SRCPATH/$SRCFILE" "$HERE/samples/symbols/$SRCFILE.symbols")

SRC_USYM=$(cat "$SRCPATH/$SRCFILE"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)

printf "; %15s %12s %12s %10s\n" \
	"Source text" "proxy K(s)" "Unique syms" "K(s)/U(s)"
printf "= %15s %12d %12d %10s\n" \
	$SRCFILE $K_OF_S $SRC_USYM `echo "scale=2; $K_OF_S/$SRC_USYM"|bc`
