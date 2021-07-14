#! /usr/bin/env sh

# Do an alpha conversion on the given source text and list of symbols to be converted.
#
# Symbols are converted to the form `g#` where # is a decimal number having just
# enough digits so that each converted symbol has its own short name.
#
# Report the compressed length of the alpha-converted text; this establishes a lower
# bound on the proxy metric for the source text's Kolmogorov complexity.

[ $# -lt 2 ] && {
	cat <<EOD
usage: `basename $0` [-s SUFFIX] SOURCE_TEXT SYMBOLS

Estimate the Kolmogorov complexity of the source text.

The source text must name a C program. The text should not have comments.

The symbols file is a list of identifiers to be alpha-converted; this
list must match the symbols present in the C program.

Strings in the program are converted uniformly to 'txt' or "txt".
While the correct operation of the program depends upon the original
string contents, the program's essential complexity does not.

The suffix option specifies a string to be appended to the minimal-length
alpha-converted identifiers. Use this to demonstrate that identifier
length has a significant impact on the complexity as estimated by
compression.
EOD
	exit 1
}

while getopts "x:" opt; do
	case $opt in
	x) suffix=$OPTARG ;;
	*) exit 1 ;;
	esac
done
shift $(($OPTIND-1))

source=$1
srcsym=$2

symlen=`cat $srcsym|wc -l|tr -d "\n"|wc -c`
edits=`mktemp`

echo 's/"[^"]*"/"txt"/g' >> $edits
echo "s/'[^']*'/'txt'/g" >> $edits
count=0
while read symbol; do
	gensym=`printf "g%.${symlen}d" $count`
	count=$(($count+1))
	echo "s/\([[:punct:][:space:]]\)$symbol\([[:punct:][:space:]]\)/\1$gensym$suffix\2/g"
done < $srcsym >> $edits

tmp=`mktemp`
cat $edits | awk '{ print length(), $0|"sort -nr"}' | cut -d' ' -f2- > $tmp
mv $tmp $edits

sed -f $edits $source|compress -c|wc -c

rm -f $edits
