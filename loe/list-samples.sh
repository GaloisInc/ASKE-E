#! /usr/bin/env sh

pr -D '' -F -l 999 samples/*.*|tr -d "\f"|sed 's/ \+Page .*$//'
