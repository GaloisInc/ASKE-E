#! /usr/bin/env sh

function report () {
	printf "%s\t%s\n" `./alpha.sh $1 $2 toposort.symbols` $2
}

echo Minimal-length alpha-converted identifiers
report "" toposort-orig.c
report "" toposort-shuf-1.c
report "" toposort-shuf-2.c
report "" toposort-shuf-3.c

echo Suffix xy appended to alpha-converted identifiers
report "-x xy" toposort-orig.c
report "-x xy" toposort-shuf-1.c
report "-x xy" toposort-shuf-2.c
report "-x xy" toposort-shuf-3.c

echo Suffix xyz appended to alpha-converted identifiers
report "-x xyz" toposort-orig.c
report "-x xyz" toposort-shuf-1.c
report "-x xyz" toposort-shuf-2.c
report "-x xyz" toposort-shuf-3.c

echo Suffix xyzzy appended to alpha-converted identifiers
report "-x xyzzy" toposort-orig.c
report "-x xyzzy" toposort-shuf-1.c
report "-x xyzzy" toposort-shuf-2.c
report "-x xyzzy" toposort-shuf-3.c

echo Suffix xyzzy0987654321 appended to alpha-converted identifiers
report "-x xyzzy0987654321" toposort-orig.c
report "-x xyzzy0987654321" toposort-shuf-1.c
report "-x xyzzy0987654321" toposort-shuf-2.c
report "-x xyzzy0987654321" toposort-shuf-3.c
