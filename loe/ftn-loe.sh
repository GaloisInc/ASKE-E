#! /usr/bin/env sh

# Compare the compressed sizes of a FORTRAN source text and its corresponding
# assembler code (as compiled; not necessarily the same a human would
# generate.
#
# System includes are assumed to be present in their default locations.
# If user includes are specified (via #include "..."), their locations
# are specified as extra command-line arguments relative to the path to
# the .c source file.
#
# We also count the number of unique symbols (excluding punctuation) in
# both the C and assembler texts as an approximation of the number of
# distinct identifiers and operators; this can be taken as an indicator
# of the number of concepts to be understood and applied by the programmer.

FFILE=$(basename "$1")
FPATH=$(dirname "$1")
TMPDIR=`mktemp -d`
NOCMT_F="nocmt-$FFILE"
ASMFILE="$NOCMT_F.s"

USRINCL=
while true; do
	shift
	[ -z "$1" ] && break
	USRINCL="$USRINCL -I$FPATH/$1"
done

cat "$FPATH/$FFILE" | grep -v '^!.*$' > "$TMPDIR/$NOCMT_F"
gcc -fpic -O0 -fno-toplevel-reorder -J$TMPDIR $USRINCL -c -S "$TMPDIR/$NOCMT_F" -o "$TMPDIR/$ASMFILE"

F_CMP=$(compress -cf "$TMPDIR/$NOCMT_F"|wc -c)
S_CMP=$(compress -cf "$TMPDIR/$ASMFILE"|wc -c)
printf "Essential\n .ftn  compressed: %6d\n .s    compressed: %6d\n" $F_CMP $S_CMP
ESSENTIAL=$(echo "scale=2; $S_CMP/$F_CMP"|bc)
printf " ratio = %9.2f\n" $ESSENTIAL

F_USYM=$(cat "$TMPDIR/$NOCMT_F"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)
S_USYM=$(cat "$TMPDIR/$ASMFILE"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)
printf "Cognitive\n .ftn  unique sym: %6d\n .s    unique sym: %6s\n" $F_USYM $S_USYM
COGNITIVE=$(echo "scale=2; $S_USYM/$F_USYM"|bc)
printf " ratio = %9.2f\n" $COGNITIVE

PRODUCT=$(echo "scale=4; $ESSENTIAL*$COGNITIVE"|bc)
printf "Product\n ratio = %9.2f\n" $PRODUCT
rm -rf $TMPDIR
