#! /usr/bin/env sh

# Compare the compressed sizes of a source text and its corresponding
# assembler code (as compiled; not necessarily the same a human would
# generate. Comments are stripped stripped from the source. Includes
# and macros (where supported by the source language) are not expanded
# before analysis.
#
# System #includes (where supported by the source language) are assumed
# to be present in their default locations. If user #includes are used,
# their locations are specified as trailing command-line arguments
# relative to the path to the source file.
#
# We count the number of unique symbols (excluding punctuation) in
# both the source and assembler texts as an approximation of the number of
# distinct identifiers and operators; this can be taken as an indicator
# of the number of concepts to be understood and applied by the programmer.

PGM=$(basename "$0")
[ $# -gt 0 ] || {
	echo "usage: $PGM SOURCEFILE [USER_INCLUDE_RELATIVE ...]"
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
EXTENSION=$(echo "$SRCFILE"|sed 's/^.*\.//')
TMPDIR=`mktemp -d`
NOCMT_SRC="nocmt-$SRCFILE"
ASMFILE="$NOCMT_SRC.s"

USRINCL=
while true; do
	shift
	[ -z "$1" ] && break
	USRINCL="$USRINCL -I$SRCPATH/$1"
done

process_c () {
	gcc -fpreprocessed -dD -E -o "$TMPDIR/$NOCMT_SRC" "$SRCPATH/$SRCFILE"
	gcc -fpic -O0 -fno-toplevel-reorder $USRINCL -c -S "$TMPDIR/$NOCMT_SRC" -o "$TMPDIR/$ASMFILE"
}

process_cpp () {
	g++ -fpreprocessed -dD -E -o "$TMPDIR/$NOCMT_SRC" "$SRCPATH/$SRCFILE"
	g++ -fpic -O0 -fno-toplevel-reorder $USRINCL -c -S "$TMPDIR/$NOCMT_SRC" -o "$TMPDIR/$ASMFILE"
}

process_ftn () {
	cat "$SRCPATH/$SRCFILE" | grep -v '^!.*$' > "$TMPDIR/$NOCMT_SRC"
	gcc -fpic -O0 -fno-toplevel-reorder -J$TMPDIR $USRINCL -c -S "$TMPDIR/$NOCMT_SRC" -o "$TMPDIR/$ASMFILE"
}

process_hs () {
	cp "$SRCPATH/$SRCFILE" "$TMPDIR/$NOCMT_SRC"  ## FINISH: strip inline and block comments
	ghc -O -ddump-asm "$TMPDIR/$NOCMT_SRC" | grep -v '^====' > "$TMPDIR/$ASMFILE"
}

case $EXTENSION in
	c) process_c ;;
	cpp) process_cpp ;;
	f90) process_ftn ;;
	hs) process_hs ;;
	*) echo "Unrecognized extension: .$EXTENSION"; exit 1 ;;
esac

SRC_CMP=$(compress -cf "$TMPDIR/$NOCMT_SRC"|wc -c)
ASM_CMP=$(compress -cf "$TMPDIR/$ASMFILE"|wc -c)
ESSENTIAL=$(echo "scale=2; $ASM_CMP/$SRC_CMP"|bc)

SRC_USYM=$(cat "$TMPDIR/$NOCMT_SRC"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)
ASM_USYM=$(cat "$TMPDIR/$ASMFILE"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)
COGNITIVE=$(echo "scale=2; $ASM_USYM/$SRC_USYM"|bc)

PRODUCT=$(echo "scale=4; $ESSENTIAL*$COGNITIVE"|bc)

rm -rf $TMPDIR

printf "; %15s %23s %23s %9s\n" \
	"Source text" "Compressed Size  " "Unique Symbols   " Product
printf "; %15s %6s %6s %9s %6s %6s %9s %9s\n" \
	"" src asm ratio src asm ratio ratio
printf "= %15s %6d %6d %9.2f %6d %6d %9.2f %9.2f\n" \
	$SRCFILE $SRC_CMP $ASM_CMP $ESSENTIAL $SRC_USYM $ASM_USYM $COGNITIVE $PRODUCT
