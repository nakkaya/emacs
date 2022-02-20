#!/usr/bin/env bash

rm -f ~/.emacs
echo "(load-file \"`pwd`/../../init.el\")" > ~/.emacs

brew install automake poppler libtool
# brew install python3
pip3 install jupyterlab
