#!/usr/bin/env bash

sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt-get install emacs27

rm -f ~/.emacs
echo "(load-file \"`pwd`/../../init.el\")" > ~/.emacs

sudo adduser $USER dialout
sudo adduser $USER dialout

gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps']"
