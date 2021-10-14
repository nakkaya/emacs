#!/bin/bash

if [[ -v PASSWD ]]; then
    echo $USER:$PASSWD | sudo chpasswd
fi

if [[ -v AWS_ACCESS_KEY_ID ]]; then
    echo "export AWS_ACCESS_KEY_ID=${AWS_ACCESS_KEY_ID}" >> /home/$USER/.bashrc
    echo "export AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY}" >> /home/$USER/.bashrc
fi

if [[ -v MLFLOW_SERVER_ENB ]]; then
    echo "export MLFLOW_TRACKING_URI=http://127.0.0.1:8080" >> /home/$USER/.bashrc

    su -c "cd /storage && mlflow server -h 0.0.0.0 -p 8080 --backend-store-uri sqlite:///mlflow.db --default-artifact-root s3://mlflow.nakkaya.com/ &" -m "$USER"
fi

/usr/sbin/sshd -D
