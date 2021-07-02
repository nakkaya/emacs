#!/bin/bash
set -e
cd /opt/emacsd

export EMACS_SERVER_SOCKET=${TMPDIR:-/tmp}/emacs$(id -u)/emacsd

emacs --daemon=$EMACS_SERVER_SOCKET &> /opt/emacsd/emacsd.log &

gotty \
    --permit-write \
    --reconnect \
    emacsclient -s ${EMACS_SERVER_SOCKET} --tty &> /opt/emacsd/gotty.log &

DISPLAY=42
ADDR=0.0.0.0:9090

xpra \
    --socket-dir=/tmp/xprad/ \
    start :$DISPLAY \
    --bind-tcp=$ADDR,auth=env \
    --html=on \
    --microphone=no \
    --pulseaudio=no \
    --speaker=no \
    --webcam=no \
    --xsettings=no \
    --clipboard=yes \
    --file-transfer=on \
    --mdns=no \
    --printing=no \
    --no-daemon \
    --start="emacs"
