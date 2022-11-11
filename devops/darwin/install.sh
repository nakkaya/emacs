#!/usr/bin/env bash

rm -f ~/.emacs
echo "(load-file \"`pwd`/../../init.el\")" > ~/.emacs

brew install automake poppler libtool aspell
brew install python@3.9
# pip3 install jupyterlab

brew tap railwaycat/emacsmacport
brew install emacs-mac --with-native-comp --with-xwidgets --with-natural-title-bar

osascript -e 'tell application "Finder" to make alias file to POSIX file "/opt/homebrew/opt/emacs-mac/Emacs.app" at POSIX file "/Applications"'
