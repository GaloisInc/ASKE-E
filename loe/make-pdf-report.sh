#! /usr/bin/env sh

text=`mktemp`
awk '/^FILL_USING / {$1=""; system($0); next} {print $0}' \
	<LOE-Metric-July.txt >$text
pandoc --pdf-engine=weasyprint \
	--metadata title="ASKE-E LOE Metric (July)" \
	-o LOE-Metric-July.pdf \
	-f markdown $text
rm -f $text
