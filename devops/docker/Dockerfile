FROM ghcr.io/nakkaya/emacsd:latest AS build

ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    LC_ALL=C.UTF-8 \
    DEBIAN_FRONTEND=noninteractive

ENV USER="core" \
    UID=1000 \
    TZ=UTC

USER root

RUN echo '#!/bin/bash' >> /usr/local/bin/cleanup_apt_cache && \
    echo 'apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* && \
    apt-get autoremove -y && \
    apt-get autoclean && \
    apt-get clean;' >> /usr/local/bin/cleanup_apt_cache && \
    chmod +x /usr/local/bin/cleanup_apt_cache

# Install Packages
#
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install curl apt-utils \
    -y --no-install-recommends && \
    cleanup_apt_cache

RUN apt-get update && \
    apt-get install \
    # apt
    gnupg software-properties-common \
    # Misc
    openssh-server sudo iputils-ping bash-completion \
    zip unzip wget htop xz-utils nq \
    graphviz qutebrowser \
    libpq-dev postgresql-client python3-psycopg2 \
    nfs-common jsonnet \
    offlineimap dovecot-imapd dnsutils nano iproute2 \
    meshlab rlwrap netcat less jq dcm2niix \
    python3-venv python3-wheel \
    # Backup & Storage
    rsync rclone git git-lfs git-annex git-annex-remote-rclone \
    apt-transport-https apache2-utils \
    # Java
    openjdk-11-jdk maven visualvm  \
    # C/C++
    build-essential gcc-10 g++-10 \
    # clang clangd cmake cppcheck valgrind \
    # protobuf-compiler protobuf-compiler-grpc-java-plugin \
    # Latex
    # texlive-latex-base texlive-xetex texlive-lang-english texlive-lang-european \
    # texlive-luatex texlive-plain-generic texlive-fonts-recommended pandoc latexmk \
    # PDF Tools
    libpng-dev zlib1g-dev libpoppler-glib-dev \
    poppler-utils libpoppler-private-dev imagemagick \
    # Vterm
    cmake libvterm-dev \
    # for cv2
    libgl1 libglib2.0-0 \
    # Jupyter
    jupyter jupyter-notebook \
    # emacs-jupyter
    autoconf automake libtool \
    -y --no-install-recommends && \
    cleanup_apt_cache

RUN apt-get update && \
    apt-get install ispell -y && \
    cleanup_apt_cache

# Node
#
RUN curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | \
    gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_20.x nodistro main" |  \
    tee /etc/apt/sources.list.d/nodesource.list && \
    apt-get update && \
    apt-get install nodejs -y --no-install-recommends && \
    cleanup_apt_cache

# K8S
#
RUN curl -fsSL https://pkgs.k8s.io/core:/stable:/v1.30/deb/Release.key | \
    sudo gpg --dearmor -o /etc/apt/keyrings/kubernetes-apt-keyring.gpg && \
    echo \
    "deb [signed-by=/etc/apt/keyrings/kubernetes-apt-keyring.gpg] https://pkgs.k8s.io/core:/stable:/v1.30/deb/ /" | \
    tee /etc/apt/sources.list.d/kubernetes.list > /dev/null && \
    apt-get update && \
    apt-get install kubectl socat lsof -y --no-install-recommends && \
    cleanup_apt_cache

# CNPG
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://github.com/cloudnative-pg/cloudnative-pg/releases/download/v1.23.1/kubectl-cnpg_1.23.1_linux_x86_64.deb' ;; \
            arm64) URL='https://github.com/cloudnative-pg/cloudnative-pg/releases/download/v1.23.1/kubectl-cnpg_1.23.1_linux_arm64.deb' ;; \
    esac; \
    curl -L -s "${URL}" -o "kubectl-cnpg.deb" && \
    dpkg -i kubectl-cnpg.deb && \
    cleanup_apt_cache

# k9s
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://github.com/derailed/k9s/releases/download/v0.32.5/k9s_Linux_amd64.tar.gz' ;; \
            arm64) URL='https://github.com/derailed/k9s/releases/download/v0.32.5/k9s_Linux_arm64.tar.gz' ;; \
    esac; \
    curl -L -s "${URL}" -o "k9s.tar.gz" && \
    tar -xzf k9s.tar.gz --directory=/usr/bin k9s && \
    rm -rf k9s.tar.gz

# nats-cli
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://github.com/nats-io/natscli/releases/download/v0.1.4/nats-0.1.4-amd64.deb' ;; \
            arm64) URL='https://github.com/nats-io/natscli/releases/download/v0.1.4/nats-0.1.4-arm64.deb' ;; \
    esac; \
    curl -L -s "${URL}" -o "nats.deb" && \
    dpkg -i nats.deb && \
    rm -rf nats.deb

# temporal-cli
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://temporal.download/cli/archive/latest?platform=linux&arch=amd64' ;; \
            arm64) URL='https://temporal.download/cli/archive/latest?platform=linux&arch=arm64' ;; \
    esac; \
    curl -L -s "${URL}" -o "temporal.tar.gz" && \
    tar -xzf temporal.tar.gz --directory=/usr/bin temporal && \
    rm -rf temporal.tar.gz

# JuiceFS
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://github.com/juicedata/juicefs/releases/download/v1.2.0/juicefs-1.2.0-linux-amd64.tar.gz' ;; \
            arm64) URL='https://github.com/juicedata/juicefs/releases/download/v1.2.0/juicefs-1.2.0-linux-arm64.tar.gz' ;; \
    esac; \
    curl -L -s "${URL}" -o "juicefs.tar.gz" && \
    tar -xzf juicefs.tar.gz --directory=/usr/bin juicefs && \
    rm -rf juicefs.tar.gz

# Terraform
#
RUN ARCH="$(dpkg --print-architecture)"; \
    TERRAFORM_VERSION=1.8.5; \
    TERRAFORM_LS_VERSION=0.33.2; \
    TERRAFORM_DIST="terraform_${TERRAFORM_VERSION}_linux_${ARCH}.zip"; \
    TERRAFORM_LS_DIST="terraform-ls_${TERRAFORM_LS_VERSION}_linux_${ARCH}.zip"; \
    echo $TERRAFORM_DIST && \
    echo $TERRAFORM_LS_DIST && \
    wget -q https://releases.hashicorp.com/terraform/${TERRAFORM_VERSION}/${TERRAFORM_DIST} && \
    wget -q https://releases.hashicorp.com/terraform-ls/${TERRAFORM_LS_VERSION}/${TERRAFORM_LS_DIST} && \
    unzip -o ${TERRAFORM_DIST} -d /usr/bin && \
    rm -rf ${TERRAFORM_DIST} && \
    unzip -o ${TERRAFORM_LS_DIST} -d /usr/bin && \
    rm -rf ${TERRAFORM_LS_DIST} && \
    rm /usr/bin/LICENSE.txt 

# Docker CLI
#
RUN ARCH="$(dpkg --print-architecture)"; \
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | \
    gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg && \
    echo \
    "deb [arch=${ARCH} signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
    $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null && \
    apt-get update && \
    apt-get install \
    docker-ce-cli \
    docker-buildx-plugin \
    docker-compose-plugin \
    -y --no-install-recommends && \
    touch /var/run/docker.sock && \
    groupadd docker && \
    chown root:docker /var/run/docker.sock && \
    usermod -a -G docker $USER && \
    cleanup_apt_cache

# Syncthing
#
RUN curl -fsSL https://syncthing.net/release-key.txt | \
    sudo gpg --dearmor -o /etc/apt/keyrings/syncthing-apt-keyring.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/syncthing-apt-keyring.gpg] https://apt.syncthing.net/ syncthing stable" | \
    tee /etc/apt/sources.list.d/syncthing.list && \
    apt-get update  && \
    apt-get install syncthing -y --no-install-recommends && \
    sed -i '/default_backend xpra/i \ \ \ \ acl is_syncthing path_beg /syncthing\n \ \ \ \use_backend syncthing if is_syncthing\n' /etc/haproxy/haproxy.cfg && \
    printf "\n\nbackend syncthing\n    mode http\n    http-request auth unless { http_auth(logins) }\n    http-request redirect location /syncthing/ if { path /syncthing }\n    http-request set-path %%[path,regsub(^/syncthing/,/)]\n    http-request set-header Host localhost\n    server syncthing_server 127.0.0.1:8384\n" >> /etc/haproxy/haproxy.cfg && \
    cleanup_apt_cache

# Configure Python
#
RUN python3 -m venv /opt/python-lsp-server && \
    /opt/python-lsp-server/bin/python -m pip install python-lsp-server[all] --no-cache-dir

RUN python3 -m venv /opt/ansible && \
    /opt/ansible/bin/python -m pip install ansible ansible-lint kubernetes --no-cache-dir && \
    /opt/ansible/bin/ansible-galaxy collection install community.kubernetes && \
    echo "export PATH=\"\$PATH:/opt/ansible/bin\"" >> /home/$USER/.bashrc

RUN python3 -m venv /opt/invoke && \
    /opt/invoke/bin/python -m pip install invoke --no-cache-dir && \
    echo "export PATH=\"\$PATH:/opt/invoke/bin\"" >> /home/$USER/.bashrc

RUN python3 -m venv /opt/aider && \
    /opt/aider/bin/python -m pip install aider-chat --no-cache-dir && \
    echo "export PATH=\"\$PATH:/opt/aider/bin\"" >> /home/$USER/.bashrc

# Miniconda
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://repo.anaconda.com/miniconda/Miniconda3-py310_24.4.0-0-Linux-x86_64.sh' ;; \
            arm64) URL='https://repo.anaconda.com/miniconda/Miniconda3-py310_24.4.0-0-Linux-aarch64.sh' ;; \
    esac; \
    curl -s "${URL}" -o "Miniconda3.sh" && \
    bash Miniconda3.sh -b -u -p /opt/conda && \
    su - $USER -c '/opt/conda/bin/conda init bash' && \
    su - $USER -c '/opt/conda/bin/conda config --set auto_activate_base false' && \
    rm -rf Miniconda3.sh

# Clojure
#
RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -P /usr/bin/ && \
    chmod 755 /usr/bin/lein && \
    /bin/bash -c "$(curl -fsSL https://download.clojure.org/install/linux-install-1.11.1.1113.sh)" && \
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)" -- --static --version 2023.10.30-16.25.41 && \
    /bin/bash -c "$(curl -s https://raw.githubusercontent.com/babashka/babashka/master/install)" && \
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo)" && \
    rm -f /usr/local/bin/clj && \
    ln -s /usr/local/bin/clojure /usr/local/bin/clj


# AWS CLI
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

# Google Cloud CLI
#
RUN curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | \
    sudo gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg && \
    echo \
    "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | \
    tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && \
    apt-get update && \
    apt-get install google-cloud-cli -y --no-install-recommends && \
    cleanup_apt_cache

# Azure CLI
#

RUN curl -sL https://aka.ms/InstallAzureCLIDeb | bash && \
    cleanup_apt_cache


# pgAdmin
#
RUN ARCH="$(dpkg --print-architecture)"; \
    mkdir /var/lib/pgadmin && \
    mkdir /var/log/pgadmin && \
    chown $USER /var/lib/pgadmin && \
    chown $USER /var/log/pgadmin && \
    python3 -m venv /opt/pgadmin && \
    /opt/pgadmin/bin/python -m pip install wheel --no-cache-dir && \
    /opt/pgadmin/bin/python -m pip install pgadmin4 --no-cache-dir && \
    sed -i -e "s/^DEFAULT_SERVER =.*/DEFAULT_SERVER=\'0\.0\.0\.0\'/g" /opt/pgadmin/lib/python3.10/site-packages/pgadmin4/config.py && \
    sed -i 's/DEFAULT_SERVER_PORT = 5050/DEFAULT_SERVER_PORT = 49162/' /opt/pgadmin/lib/python3.10/site-packages/pgadmin4/config.py && \
    sed -i 's/SERVER_MODE = True/SERVER_MODE = False/' /opt/pgadmin/lib/python3.10/site-packages/pgadmin4/config.py && \
    sed -i 's/ENHANCED_COOKIE_PROTECTION = True/ENHANCED_COOKIE_PROTECTION = False/' /opt/pgadmin/lib/python3.10/site-packages/pgadmin4/config.py && \
    sed -i 's/MASTER_PASSWORD_REQUIRED = True/MASTER_PASSWORD_REQUIRED = False/' /opt/pgadmin/lib/python3.10/site-packages/pgadmin4/config.py && \
    mkdir -p /etc/pgadmin && \
    echo "DATA_DIR = '/home/${USER}/.pgadmin/'" >> /etc/pgadmin/config_system.py && \
    mkdir -p /home/$USER/.pgadmin && \
    sed -i '/default_backend xpra/i \ \ \ \ acl is_pgadmin path_beg /pgadmin\n \ \ \ \use_backend pgadmin if is_pgadmin\n' /etc/haproxy/haproxy.cfg && \
    printf "\n\nbackend pgadmin\n    mode http\n    http-request auth unless { http_auth(logins) }\n    http-request set-header X-Script-Name /pgadmin\n    server pgadmin_server 127.0.0.1:49162\n" >> /etc/haproxy/haproxy.cfg


RUN mkdir /opt/skypilot && \
    chown $USER:$USER /opt/skypilot && \
    su - $USER -c 'python3 -m venv /opt/skypilot' && \
    su - $USER -c '/opt/skypilot/bin/python -m pip install "skypilot[aws,kubernetes,runpod,gcp]==0.6.1" --no-cache-dir' && \
    echo "export PATH=\"\$PATH:/opt/skypilot/bin\"" >> /home/$USER/.bashrc

# OpenVSCode Server
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://github.com/gitpod-io/openvscode-server/releases/download/openvscode-server-v1.90.1/openvscode-server-v1.90.1-linux-x64.tar.gz' ;; \
            arm64) URL='https://github.com/gitpod-io/openvscode-server/releases/download/openvscode-server-v1.90.1/openvscode-server-v1.90.1-linux-arm64.tar.gz' ;; \
    esac; \
    cd /opt/ && \
    curl -L -s "${URL}" -o "vscode.tar.gz" && \
    mkdir openvscode-server && \
    tar xf vscode.tar.gz -C openvscode-server --strip-components=1 && \
    rm vscode.tar.gz && \
    sed -i '/default_backend xpra/i \ \ \ \ acl is_vscode path_beg /vscode\n \ \ \ \use_backend vscode if is_vscode\n' /etc/haproxy/haproxy.cfg && \
    printf "\n\nbackend vscode\n    mode http\n    http-request auth unless { http_auth(logins) }\n    server vscode_server 127.0.0.1:49164\n" >> /etc/haproxy/haproxy.cfg

# Setup Jupter Reverse Proxy
#
RUN sed -i '/default_backend xpra/i \ \ \ \ acl is_jupyter path_beg /jupyter\n \ \ \ \use_backend jupyter if is_jupyter\n' /etc/haproxy/haproxy.cfg && \
    printf "\n\nbackend jupyter\n    mode http\n    http-request auth unless { http_auth(logins) }\n    server jupyter_server 127.0.0.1:49166\n" >> /etc/haproxy/haproxy.cfg

# Setup Emacs
#
RUN git clone https://github.com/nakkaya/emacs /opt/emacsd/conf && \
    echo "(setq package-native-compile t)" > /home/$USER/.emacs && \
    echo "(load-file \"/opt/emacsd/conf/init.el\")" >> /home/$USER/.emacs

RUN echo 'PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/opt/conda/bin:/opt/python-lsp-server/bin:/opt/skypilot/bin:/opt/ansible/bin:/opt/invoke/bin:"' > /etc/environment

COPY resources/bin/ob-tangle.sh /usr/bin/ob-tangle
RUN sudo chmod +x /usr/bin/ob-tangle
COPY resources/bin/bootrc /usr/bin/bootrc
RUN sudo chmod +x /usr/bin/bootrc
COPY resources/conf/supervisord/bootrc.conf /etc/supervisor/conf.d/
COPY resources/conf/supervisord/syncthing.conf /etc/supervisor/conf.d/
COPY resources/conf/supervisord/pgadmin.conf /etc/supervisor/conf.d/
COPY resources/conf/supervisord/jupyter.conf /etc/supervisor/conf.d/
COPY resources/conf/supervisord/vscode.conf /etc/supervisor/conf.d/

RUN mkdir -p /home/$USER/.local/share/ && \
    chown -R $USER:$USER /opt/emacsd && \
    chown -R $USER:$USER /home/$USER && \
    chown -R $USER:$USER /storage

USER $USER
