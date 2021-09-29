#!/bin/bash
set -e

export TERM=xterm-256color

if [[ -v PASSWD ]]; then
    echo $USER:$PASSWD | sudo chpasswd
fi

sudo /usr/sbin/sshd

wait
