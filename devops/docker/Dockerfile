FROM ghcr.io/nakkaya/emacsd:latest as build

ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    LC_ALL=C.UTF-8 \
    DEBIAN_FRONTEND=noninteractive

ENV USER="core" \
    UID=1000 \
    TZ=UTC

USER root

# Install Packages
#
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install curl apt-utils -y --no-install-recommends && \
    curl -sL https://deb.nodesource.com/setup_16.x | sudo -E bash -

RUN apt-get install \
    # apt
    gnupg software-properties-common \
    # Misc
    openssh-server sudo iputils-ping bash-completion \
    zip unzip wget htop xz-utils nq \
    graphviz qutebrowser \
    libpq-dev postgresql-client influxdb-client python3-psycopg2 \
    redis-tools \
    offlineimap dovecot-imapd dnsutils nano iproute2 \
    meshlab rlwrap netcat less jq \
    python3-venv python3-wheel \
    # Backup & Storage
    rsync rclone git git-lfs git-annex git-annex-remote-rclone \
    apt-transport-https apache2-utils \
    # Java
    openjdk-11-jdk maven visualvm  \
    # C/C++
    build-essential gcc-10 g++-10 clang clangd cmake cppcheck valgrind \
    protobuf-compiler protobuf-compiler-grpc-java-plugin \
    # Latex
    texlive-latex-base texlive-xetex texlive-lang-english texlive-lang-european \
    texlive-luatex texlive-plain-generic texlive-fonts-recommended pandoc latexmk \
    # PDF Tools
    libpng-dev zlib1g-dev libpoppler-glib-dev \
    poppler-utils libpoppler-private-dev imagemagick \
    # for cv2
    libgl1 libglib2.0-0 \
    # Jupyter
    jupyter jupyter-notebook \
    # emcas-jupyter
    autoconf automake libtool \
    # For Teensy
    # libxft2 \
    tesseract-ocr \
    -y --no-install-recommends

RUN apt-get install ispell -y

# Node
#
RUN apt-get install -y nodejs

# K8S
#
RUN curl -fsSL https://pkgs.k8s.io/core:/stable:/v1.28/deb/Release.key | \
    sudo gpg --dearmor -o /etc/apt/keyrings/kubernetes-apt-keyring.gpg && \
    echo \
    "deb [signed-by=/etc/apt/keyrings/kubernetes-apt-keyring.gpg] https://pkgs.k8s.io/core:/stable:/v1.28/deb/ /" \
    | tee /etc/apt/sources.list.d/kubernetes.list > /dev/null && \
    apt-get update && \
    apt-get install kubectl socat -y --no-install-recommends

# Install Serverless
#

RUN npm install -g serverless

# Install Terraform
#
RUN ARCH="$(dpkg --print-architecture)"; \
    TERRAFORM_VERSION=0.14.11; \
    TERRAFORM_LS_VERSION=0.22.0; \
    TERRAFORM_DIST="terraform_${TERRAFORM_VERSION}_linux_${ARCH}.zip"; \
    TERRAFORM_LS_DIST="terraform-ls_${TERRAFORM_LS_VERSION}_linux_${ARCH}.zip"; \
    echo $TERRAFORM_DIST && \
    echo $TERRAFORM_LS_DIST && \
    wget -q https://releases.hashicorp.com/terraform/${TERRAFORM_VERSION}/${TERRAFORM_DIST} && \
    wget -q https://releases.hashicorp.com/terraform-ls/${TERRAFORM_LS_VERSION}/${TERRAFORM_LS_DIST} && \
    unzip ${TERRAFORM_DIST} -d /usr/bin && \
    rm -rf ${TERRAFORM_DIST} && \
    unzip ${TERRAFORM_LS_DIST} -d /usr/bin && \
    rm -rf ${TERRAFORM_LS_DIST}

# Install Docker CLI
#
RUN ARCH="$(dpkg --print-architecture)"; \
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | \
    gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg && \
    echo \
    "deb [arch=${ARCH} signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
    $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null && \
    apt-get update && \
    apt-get install docker-ce-cli -y --no-install-recommends && \
    curl -L "https://github.com/docker/compose/releases/download/v2.12.2/docker-compose-linux-$(uname -m)" -o /usr/local/bin/docker-compose && \
    chmod +x  /usr/local/bin/docker-compose && \
    touch /var/run/docker.sock && \
    groupadd docker && \
    chown root:docker /var/run/docker.sock && \
    usermod -a -G docker $USER

# Install Syncthing
#
RUN wget -q https://syncthing.net/release-key.txt -O- | apt-key add - && \
    add-apt-repository "deb https://apt.syncthing.net/ syncthing stable" && \
    apt-get install syncthing -y --no-install-recommends

# Configure Python
#

RUN python3 -m venv /opt/python-lsp-server && \
    /opt/python-lsp-server/bin/python -m pip install python-lsp-server[all] --no-cache-dir

RUN python3 -m venv /opt/ansible && \
    /opt/ansible/bin/python -m pip install ansible ansible-lint --no-cache-dir && \
    echo "export PATH=\"\$PATH:/opt/ansible/bin\"" >> /home/$USER/.bashrc

RUN python3 -m venv /opt/invoke && \
    /opt/invoke/bin/python -m pip install invoke --no-cache-dir && \
    echo "export PATH=\"\$PATH:/opt/invoke/bin\"" >> /home/$USER/.bashrc

# Install Miniconda
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://repo.anaconda.com/miniconda/Miniconda3-py310_23.1.0-1-Linux-x86_64.sh' ;; \
            arm64) URL='https://repo.anaconda.com/miniconda/Miniconda3-py310_23.1.0-1-Linux-aarch64.sh' ;; \
    esac; \
    curl -s "${URL}" -o "Miniconda3.sh" && \
    bash Miniconda3.sh -b -u -p /opt/conda && \
    su - $USER -c '/opt/conda/bin/conda init bash' && \
    su - $USER -c '/opt/conda/bin/conda config --set auto_activate_base false' && \
    rm -rf Miniconda3.sh

# Install Clojure
#
RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -P /usr/bin/ && \
    chmod 755 /usr/bin/lein && \
    /bin/bash -c "$(curl -fsSL https://download.clojure.org/install/linux-install-1.11.1.1113.sh)" && \
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)" -- --static && \
    /bin/bash -c "$(curl -s https://raw.githubusercontent.com/babashka/babashka/master/install)" && \
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo)" && \
    rm -f /usr/local/bin/clj && \
    ln -s /usr/local/bin/clojure /usr/local/bin/clj

# Install GraalVM
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-22.3.1/graalvm-ce-java11-linux-amd64-22.3.1.tar.gz' ;; \
            arm64) URL='https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-22.3.1/graalvm-ce-java11-linux-aarch64-22.3.1.tar.gz' ;; \
    esac; \
    curl -L -s "${URL}" -o "graalvm.tar.gz" && \
    mkdir graalvm && \
    tar -xzf graalvm.tar.gz --strip 1 -C graalvm && \
    mv graalvm /opt/ && \
    /opt/graalvm/bin/gu install native-image && \
    rm graalvm.tar.gz


# Install AWS CLI
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip' ;; \
            arm64) URL='https://awscli.amazonaws.com/awscli-exe-linux-aarch64.zip' ;; \
    esac; \
    cd /opt/ && \
    curl -s "${URL}" -o "awscliv2.zip" && \
    unzip -q awscliv2.zip && \
    ./aws/install && \
    rm awscliv2.zip

# Install Google Cloud CLI
#
RUN ARCH="$(dpkg --print-architecture)"; \
    curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key --keyring /usr/share/keyrings/cloud.google.gpg add - && \
    echo \
    "deb [arch=${ARCH} signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt \
    cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && \
    apt-get update && \
    apt-get install google-cloud-sdk -y --no-install-recommends

# Install GitHub Tool
#

# RUN curl https://cli.github.com/packages/githubcli-archive-keyring.gpg  | apt-key --keyring /usr/share/keyrings/githubcli-archive-keyring.gpg add - && \
#     echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null && \
#     apt-get update && \
#     apt-get install gh -y --no-install-recommends

# Install pgAdmin
#
# /opt/pgadmin/bin/python /opt/pgadmin/lib/python3.10/site-packages/pgadmin4/setup.py
# supervisorctl restart pgadmin

RUN mkdir /var/lib/pgadmin && \
    mkdir /var/log/pgadmin && \
    chown $USER /var/lib/pgadmin && \
    chown $USER /var/log/pgadmin && \
    python3 -m venv /opt/pgadmin && \
    /opt/pgadmin/bin/python -m pip install install wheel --no-cache-dir && \
    /opt/pgadmin/bin/python -m pip install install pgadmin4 --no-cache-dir && \
    sed -i -e "s/^DEFAULT_SERVER =.*/DEFAULT_SERVER=\'0\.0\.0\.0\'/g" /opt/pgadmin/lib/python3.10/site-packages/pgadmin4/config.py && \
    mkdir -p /etc/pgadmin && \
    echo "DATA_DIR = '/home/${USER}/.pgadmin/'" >> /etc/pgadmin/config_system.py && \
    mkdir -p /home/$USER/.pgadmin

RUN AIRFLOW_VERSION="2.5.2"; \
    PYTHON_VERSION="3.10"; \
    AIRFLOW_HOME=/home/$USER/.bashrc; \
    CONSTRAINT_URL="https://raw.githubusercontent.com/apache/airflow/constraints-${AIRFLOW_VERSION}/constraints-${PYTHON_VERSION}.txt"; \
    python3 -m venv /opt/airflow && \
    /opt/airflow/bin/python -m pip install virtualenv --no-cache-dir && \
    /opt/airflow/bin/python -m pip install "apache-airflow==${AIRFLOW_VERSION}" --constraint "${CONSTRAINT_URL}" --no-cache-dir && \
    sed -i -e "s/^load_examples = .*/load_examples = False/g" /opt/airflow/lib/python3.10/site-packages/airflow/config_templates/default_airflow.cfg && \
    /opt/airflow/bin/python -m pip install "apache-airflow-providers-postgres==5.4.0" --no-cache-dir

RUN python3 -m venv /opt/skypilot && \
    /opt/skypilot/bin/python -m pip install "skypilot[gcp,aws,kubernetes]==0.3.1" --no-cache-dir && \
    echo "export PATH=\"\$PATH:/opt/skypilot/bin\"" >> /home/$USER/.bashrc

# Arduino
#
# RUN wget http://downloads.arduino.cc/arduino-1.8.13-linux64.tar.xz && \
#     tar xf arduino-1.8.13-linux64.tar.xz && \
#     mv arduino-1.8.13 /usr/local/share/arduino && \
#     ln -s /usr/local/share/arduino/arduino /usr/local/bin/arduino && \
#     ln -s /usr/local/share/arduino/arduino-builder /usr/local/bin/arduino-builder && \
#     rm -rf arduino-1.8.13-linux64.tar.xz && \
#     arduino --install-boards arduino:sam && \
#     wget https://www.pjrc.com/teensy/td_153/TeensyduinoInstall.linux64 && \
#     chmod +x TeensyduinoInstall.linux64 && \
#     ./TeensyduinoInstall.linux64  --dir=/usr/local/share/arduino && \
#     rm -rf TeensyduinoInstall.linux64

RUN apt-get autoremove -y && \
    apt-get clean && \
    apt-get autoclean

# Setup Emacs
#
RUN git clone https://github.com/nakkaya/emacs /opt/emacsd/conf && \
    echo "(setq package-native-compile t)" > /home/$USER/.emacs && \
    echo "(load-file \"/opt/emacsd/conf/init.el\")" >> /home/$USER/.emacs

COPY resources/bin/ob-tangle.sh /usr/bin/ob-tangle
RUN sudo chmod +x /usr/bin/ob-tangle
COPY resources/bin/bootrc /usr/bin/bootrc
RUN sudo chmod +x /usr/bin/bootrc
COPY resources/bin/notebook /usr/bin/notebook
RUN sudo chmod +x /usr/bin/notebook
COPY resources/conf/supervisord/bootrc.conf /etc/supervisor/conf.d/
COPY resources/conf/supervisord/syncthing.conf /etc/supervisor/conf.d/
COPY resources/conf/supervisord/pgadmin.conf /etc/supervisor/conf.d/
COPY resources/conf/supervisord/jupyter.conf /etc/supervisor/conf.d/
COPY resources/conf/supervisord/airflow.conf /etc/supervisor/conf.d/

RUN mkdir -p /home/$USER/.local/share/ && \
    chown -R $USER:$USER /opt/emacsd && \
    chown -R $USER:$USER /home/$USER && \
    chown -R $USER:$USER /storage

USER $USER
