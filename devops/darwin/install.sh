#!/usr/bin/env bash

rm -f ~/.emacs
echo "(load-file \"`pwd`/../../init.el\")" > ~/.emacs
