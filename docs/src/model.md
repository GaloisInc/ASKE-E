# `model`s

A `model` is a named entity (`CHIME` in the [example](./example.md)), followed by a set of
declarations of state and other variables, followed by a set of declarations of
events that may depend on or modify these variables, modulo some rules.

`model`s are constructed blockwise in a whitespace-sensitive way that should be
familiar to users of YAML and other similar languages.
