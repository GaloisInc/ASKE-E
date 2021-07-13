#! /usr/bin/env sh

pr -D '' -F samples/*.*|tr -d "\f"|sed 's/ \+Page .*$//'
