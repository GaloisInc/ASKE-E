#! /usr/bin/env sh

# Compare the compressed sizes of a Haskell source text and its corresponding
# assembler code (as compiled; not necessarily the same a human would
# generate.
#
# NOTE: Haskell comments are NOT stripped from the source text.
#
# We also count the number of unique symbols (excluding punctuation) in
# both the Haskell and assembler texts as an approximation of the number of
# distinct identifiers and operators; this can be taken as an indicator
# of the number of concepts to be understood and applied by the programmer.

HSFILE=$(basename "$1")
HSPATH=$(dirname "$1")
TMPDIR=`mktemp -d`
NOCMT_HS="nocmt-$HSFILE"
ASMFILE="$NOCMT_HS.s"

cp "$HSPATH/$HSFILE" "$TMPDIR/$NOCMT_HS"  ## FINISH: strip inline and block comments
ghc -O -ddump-asm "$TMPDIR/$NOCMT_HS" | grep -v '^====' > "$TMPDIR/$ASMFILE"

HS_CMP=$(compress -cf "$TMPDIR/$NOCMT_HS"|wc -c)
S_CMP=$(compress -cf "$TMPDIR/$ASMFILE"|wc -c)
printf "Essential\n .hs   compressed: %6d\n .s    compressed: %6d\n" $HS_CMP $S_CMP
ESSENTIAL=$(echo "scale=2; $S_CMP/$HS_CMP"|bc)
printf " ratio = %9.2f\n" $ESSENTIAL

HS_USYM=$(cat "$TMPDIR/$NOCMT_HS"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)
S_USYM=$(cat "$TMPDIR/$ASMFILE"|tr -cs [a-zA-Z0-9_.] '\n'|sort|uniq|wc -w)
printf "Cognitive\n .hs   unique sym: %6d\n .s    unique sym: %6s\n" $HS_USYM $S_USYM
COGNITIVE=$(echo "scale=2; $S_USYM/$HS_USYM"|bc)
printf " ratio = %9.2f\n" $COGNITIVE

PRODUCT=$(echo "scale=4; $ESSENTIAL*$COGNITIVE"|bc)
printf "Product\n ratio = %9.2f\n" $PRODUCT
rm -rf $TMPDIR
