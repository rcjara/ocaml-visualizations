#!/usr/bin/env bash

rm *.cm* 2>/dev/null

ocamlbuild -use-ocamlfind -cflag "-thread" $* 2>/dev/null
