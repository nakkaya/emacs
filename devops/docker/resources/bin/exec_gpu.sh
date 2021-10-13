#!/bin/bash

if [[ -v PASSWD ]]; then
    echo $USER:$PASSWD | sudo chpasswd
fi

if [[ -v MLFLOW_SERVER_ENB ]]; then
    ( cd /storage && mlflow server -h 0.0.0.0 -p 8080 --backend-store-uri sqlite:///mlflow.db --default-artifact-root s3://mlflow.nakkaya.com/ & )
fi

/usr/sbin/sshd -D
