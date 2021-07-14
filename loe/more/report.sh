#! /usr/bin/env sh

printf "; %15s %6s %6s\n" Source Comp Usym
for f in ode-*; do
	./loe-bare.sh $f
done
