#!/bin/bash
set -e

export EMACS_HOME_DIR=/storage/
export TERM=xterm-256color

XPRA_DISPLAY=42

if [[ -v PASSWD ]]; then
    echo $USER:$PASSWD | chpasswd

    export XPRA_PASSWORD="${PASSWD}"

    htpasswd -bc /opt/emacsd/server/htpasswd $USER $PASSWD
    export RCLONE_PASSWORD="--htpasswd /opt/emacsd/server/htpasswd"

    XPRA_ADDR="0.0.0.0:9090,auth=env"
else
    XPRA_ADDR="0.0.0.0:9090"

    export RCLONE_PASSWORD=""
fi

rclone serve \
       --addr :1010 \
       $RCLONE_PASSWORD \
       --dir-cache-time 5s \
       --dir-perms 0755 \
       --file-perms 0644 \
       webdav /storage &> /opt/emacsd/logs/webdav.log &

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

/usr/sbin/sshd -D
