#!/usr/bin/env bash

sudo apt-get update
sudo apt-get install emacs

rm -f ~/.emacs
echo "(load-file \"`pwd`/../../init.el\")" > ~/.emacs

sudo adduser $USER dialout
sudo adduser $USER dialout

gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps']"
