#! /usr/bin/env sh

pandoc --pdf-engine=weasyprint \
	--metadata title="ASKE-E LOE Metric (July)" \
	LOE-Metric-July.txt -o LOE-Metric-July.pdf
