#! /usr/bin/env sh

pr -D '' -F -l 999 bounds/toposort-*.c samples/*.*|tr -d "\f"|sed 's/ \+Page .*$//'
