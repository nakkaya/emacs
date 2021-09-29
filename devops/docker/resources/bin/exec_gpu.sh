#!/bin/bash

if [[ -v PASSWD ]]; then
    echo $USER:$PASSWD | sudo chpasswd
fi

/usr/sbin/sshd -D
