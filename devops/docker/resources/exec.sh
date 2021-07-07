#!/bin/bash
set -e

cd /opt/emacsd
mkdir logs

export EMACS_SERVER_SOCKET=${TMPDIR:-/tmp}/emacs$(id -u)/emacsd

emacs --daemon=$EMACS_SERVER_SOCKET &> /opt/emacsd/logs/emacsd.log &

gotty \
    --permit-write \
    --reconnect \
    emacsclient -s ${EMACS_SERVER_SOCKET} --tty &> /opt/emacsd/logs/gotty.log &

XPRA_DISPLAY=42

if [[ -v XPRA_PASSWORD ]]; then
    XPRA_ADDR="0.0.0.0:9090,auth=env"
else
    XPRA_ADDR="0.0.0.0:9090"
fi

xpra \
    --socket-dir=/tmp/xprad/ \
    start :$XPRA_DISPLAY \
    --bind-tcp=$XPRA_ADDR \
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
    --start="emacs" &> /opt/emacsd/logs/xpra.log &

wait
