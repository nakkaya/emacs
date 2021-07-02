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

if [[ -v XPRA_PASSWORD ]]; then
    XPRA_AUTH=",auth=env"
else
    XPRA_AUTH=""
fi

xpra \
    --socket-dir=/tmp/xprad/ \
    start :$DISPLAY \
    --bind-tcp=$ADDR$XPRA_AUTH \
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
