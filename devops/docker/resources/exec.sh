#!/bin/bash
set -e

cd /opt/emacsd
mkdir logs
mkdir server

export EMACS_HOME_DIR=/storage/
export TERM=xterm-256color

echo "(set-face-background 'default \"black\")" >> ~/.emacs 
echo "(setq server-socket-dir \"/opt/emacsd/server\")" >> ~/.emacs
echo "(setq server-name \"emacsd\")" >> ~/.emacs
echo "(defun server-ensure-safe-dir (dir) \"Noop\" t)" >> ~/.emacs
echo "(server-start)" >> ~/.emacs

XPRA_DISPLAY=42

if [[ -v PASSWD ]]; then
    export XPRA_PASSWORD="${PASSWD}"
    export GOTTY_CREDENTIAL="${USER}:${PASSWD}"

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
    --start-after-connect=no \
    --start="emacs" &> /opt/emacsd/logs/xpra.log &

#while [ ! -e /opt/emacsd/server/emacsd ]; do sleep 1; done

gotty \
    --permit-write \
    --reconnect \
    emacsclient -s /opt/emacsd/server/emacsd --tty &> /opt/emacsd/logs/gotty.log &

wait
