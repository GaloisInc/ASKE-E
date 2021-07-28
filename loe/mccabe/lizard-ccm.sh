#! /usr/bin/env sh


[ $# -eq 1 ] || {
	echo "usage: $0 FILE"
	exit 1
}
HERE=$(cd `dirname $0`; pwd)
echo "$1"
python $HERE/lizard-1.17.9/lizard.py -Eoutside -ENS "$1" \
	|sed 's/^=\+$//'|awk '/^.* file analyzed/ {exit} {print}' \
	|sed 's|^\(.*\)@.*$|\1|'
