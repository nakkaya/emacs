#!/bin/bash
set -e
export EMACS_SERVER_SOCKET=${TMPDIR:-/tmp}/emacs$(id -u)/emacsd

cd /opt/emacsd

nohup emacs --daemon=$EMACS_SERVER_SOCKET &> /opt/emacsd/emacsd.log &

gotty --permit-write --reconnect emacsclient -s ${EMACS_SERVER_SOCKET} --tty
