#!/bin/bash

if [[ -v PASSWD ]]; then
    echo $USER:$PASSWD | sudo chpasswd
fi

( cd /sandbox && mlflow server -h 0.0.0.0 -p 8080 & )

/usr/sbin/sshd -D
