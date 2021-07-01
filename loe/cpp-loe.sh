#! /usr/bin/env sh

# Compare the compressed sizes of a C++ source text and its corresponding
# assembler code (as compiled; not necessarily the same a human would
# generate. C++ comments (both styles) are stripped. Includes and macros
# are not expanded when sizing the C++ text.
#
# System includes are assumed to be present in their default locations.
# If user includes are specified (via #include "..."), their locations
# are specified as extra command-line arguments relative to the path to
# the .c source file.
#
# We also count the number of unique symbols (excluding punctuation) in
# both the C++ and assembler texts as an approximation of the number of
# distinct identifiers and operators; this can be taken as an indicator
# of the number of concepts to be understood and applied by the programmer.

CFILE=$(basename "$1")
CPATH=$(dirname "$1")
TMPDIR=`mktemp -d`
NOCMT_C="nocmt-$CFILE"
ASMFILE="$NOCMT_C.s"

USRINCL=
while true; do
	shift
	[ -z "$1" ] && break
	USRINCL="$USRINCL -I$CPATH/$1"
done

g++ -fpreprocessed -dD -E -o "$TMPDIR/$NOCMT_C" "$CPATH/$CFILE"
g++ -fpic -O0 -fno-toplevel-reorder $USRINCL -c -S "$TMPDIR/$NOCMT_C" -o "$TMPDIR/$ASMFILE"

C_CMP=$(compress -cf "$TMPDIR/$NOCMT_C"|wc -c)
S_CMP=$(compress -cf "$TMPDIR/$ASMFILE"|wc -c)
printf "Essential\n .cpp  compressed: %6d\n .s    compressed: %6d\n" $C_CMP $S_CMP
ESSENTIAL=$(echo "scale=2; $S_CMP/$C_CMP"|bc)
printf " ratio = %9.2f\n" $ESSENTIAL

C_USYM=$(cat "$TMPDIR/$NOCMT_C"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)
S_USYM=$(cat "$TMPDIR/$ASMFILE"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)
printf "Cognitive\n .cpp  unique sym: %6d\n .s    unique sym: %6s\n" $C_USYM $S_USYM
COGNITIVE=$(echo "scale=2; $S_USYM/$C_USYM"|bc)
printf " ratio = %9.2f\n" $COGNITIVE

PRODUCT=$(echo "scale=4; $ESSENTIAL*$COGNITIVE"|bc)
printf "Product\n ratio = %9.2f\n" $PRODUCT
rm -rf $TMPDIR
