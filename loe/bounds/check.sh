#! /usr/bin/env sh

HERE=$(cd `dirname $0`; pwd)
for f in $HERE/toposort-*.c; do
	echo
	echo ";;; $f"
	echo
	$HERE/alpha.sh -c $f $HERE/toposort.symbols
done
