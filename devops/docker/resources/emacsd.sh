#!/bin/bash
set -e

cd /opt/emacsd

nohup /usr/bin/emacs --daemon &> /opt/emacsd/emacsd.log &

gotty --permit-write --reconnect emacsclient --tty -a ""
