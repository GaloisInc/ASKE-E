#! /usr/bin/env sh

# Do an alpha conversion on the given source text and list of symbols to be converted.
#
# Symbols are converted to the form `g#` where # is a decimal number having just
# enough digits so that each converted symbol has its own short name.
#
# Report the compressed length of the alpha-converted text; this establishes a lower
# bound on the proxy metric for the source text's Kolmogorov complexity.

check=false

while getopts "x:c" opt; do
	case $opt in
	x) suffix=$OPTARG ;;
	c) check=true ;;
	*) exit 1 ;;
	esac
done
shift $(($OPTIND-1))

[ $# -lt 2 ] && {
	cat <<EOD
usage: `basename $0` [-s SUFFIX] [-c] SOURCE_TEXT SYMBOLS

Estimate the Kolmogorov complexity of the source text.

The source text must name a C program. The text should not have comments.

The symbols file is a list of identifiers to be alpha-converted; this
list must match the symbols present in the C program.

Strings in the program are converted uniformly to 'txt' or "txt".
While the correct operation of the program depends upon the original
string contents, the program's essential complexity does not.

The -s SUFFIX option specifies a string to be appended to the minimal-length
alpha-converted identifiers. Use this to demonstrate that identifier
length has a significant impact on the complexity as estimated by
compression.

The -c option suppresses the Kolmogorov estimate and shows the
alpha-converted text.
EOD
	exit 1
}

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

if $check; then
	sed -f $edits $source
else
	# The xz compressor with a fixed method produces slightly smaller files than does compress.
	# It does so by omitting the header data to specify the decompression method.
	##sed -f $edits $source|compress -c|wc -c
	sed -f $edits $source|xz -Fraw --lzma2=pb=0,lc=0 --stdout|wc -c
fi

rm -f $edits
