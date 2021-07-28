#! /usr/bin/env sh


[ $# -eq 1 ] || {
	echo "usage: $0 FILE"
	exit 1
}
echo "$1"
lizard -Eoutside -ENS "$1" \
	|sed 's/^=\+$//'|awk '/^.* file analyzed/ {exit} {print}' \
	|sed 's|^\(.*\)@.*$|\1|'|cut -c11-16,40-
