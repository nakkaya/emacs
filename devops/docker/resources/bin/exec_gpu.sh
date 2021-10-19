#!/bin/bash

if [[ -v PASSWD ]]; then
    echo $USER:$PASSWD | sudo chpasswd
fi

if [[ -v AWS_ACCESS_KEY_ID ]]; then
    echo "export AWS_ACCESS_KEY_ID=${AWS_ACCESS_KEY_ID}" >> /home/$USER/.bashrc
    echo "export AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY}" >> /home/$USER/.bashrc
fi

if [[ -v MLFLOW_TRACKING_URI ]]; then
    echo "export MLFLOW_TRACKING_URI=${MLFLOW_TRACKING_URI}" >> /home/$USER/.bashrc
fi

if [[ -v MLFLOW_TRACKING_USERNAME ]]; then
    echo "export MLFLOW_TRACKING_USERNAME=${MLFLOW_TRACKING_USERNAME}" >> /home/$USER/.bashrc
fi

if [[ -v MLFLOW_TRACKING_PASSWORD ]]; then
    echo "export MLFLOW_TRACKING_PASSWORD=${MLFLOW_TRACKING_PASSWORD}" >> /home/$USER/.bashrc
fi

if [[ -v MLFLOW_SERVER_ENB ]]; then
    su -c "cd /storage && mlflow server -h 0.0.0.0 -p 8080 --backend-store-uri sqlite:///mlflow.db --default-artifact-root s3://mlflow.nakkaya.com/ &" -m "$USER"
fi

/usr/sbin/sshd -D
