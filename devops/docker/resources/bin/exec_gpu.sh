#!/bin/bash

if [[ -v PASSWD ]]; then
    echo $USER:$PASSWD | sudo chpasswd
fi

if [[ -v MLFLOW_SERVER_ENB ]]; then
    ( cd /sandbox && mlflow server -h 0.0.0.0 -p 8080 & )
fi

/usr/sbin/sshd -D
