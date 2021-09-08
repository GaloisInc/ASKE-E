#! /usr/bin/env sh

for f in samples/*.*; do
	echo $f|grep -q '\.txt$' && continue
	printf "%24s %8.0f\n" \
		$f \
		$(multimetric $f|jq .files\|.\[\]\|.halstead_effort)
done
