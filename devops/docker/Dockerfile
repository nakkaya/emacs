ARG BASE_IMAGE=ghcr.io/nakkaya/emacsd-cpu
FROM $BASE_IMAGE as build

ARG IMAGE_TYPE
ENV IMAGE_TYPE=$IMAGE_TYPE

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
    apt-get install curl -y --no-install-recommends && \
    curl -sL https://deb.nodesource.com/setup_16.x | sudo -E bash -

RUN apt-get install \
    # apt
    apt-utils gnupg software-properties-common \
    # Misc
    openssh-server sudo iputils-ping bash-completion \
    zip unzip wget htop xz-utils nq \
    graphviz qutebrowser \
    libpq-dev postgresql-client python3-psycopg2 \
    redis-tools \
    offlineimap dovecot-imapd dnsutils nano iproute2 \
    meshlab rlwrap netcat less jq \
    python3-venv \
    # Backup & Storage
    rsync rclone git git-annex git-annex-remote-rclone \
    apt-transport-https apache2-utils \
    # Java
    openjdk-11-jdk maven  \
    # C/C++
    build-essential gcc-10 g++-10 clang clangd cmake cppcheck valgrind \
    # Latex
    texlive-latex-base texlive-xetex texlive-lang-english \
    texlive-lang-european texlive-plain-generic texlive-fonts-recommended \
    pandoc latexmk \
    # PDF Tools
    libpng-dev zlib1g-dev libpoppler-glib-dev \
    libpoppler-private-dev imagemagick \
    # for cv2
    libgl1 libglib2.0-0 \
    # emcas-jupyter
    autoconf automake libtool \
    # For Teensy
    # libxft2 \
    -y --no-install-recommends

RUN apt-get install ispell -y

# Node
#
RUN apt-get install -y nodejs

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
    curl -L "https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose && \
    touch /var/run/docker.sock && \
    groupadd docker && \
    chown root:docker /var/run/docker.sock && \
    usermod -a -G docker $USER

# Install Syncthing
#
RUN wget -q https://syncthing.net/release-key.txt -O- | apt-key add - && \
    add-apt-repository "deb https://apt.syncthing.net/ syncthing stable" && \
    apt-get update && \
    apt-get install syncthing -y --no-install-recommends

# Configure Python
#

ENV ML_CPU="\
    python-lsp-server[all] \
    jinja2==3.1.2 \
    invoke \
    ansible \
    ansible-lint \
    jupyterlab \
    ipywidgets \
    numpy==1.23.1 \
    numexpr \
    pandas \
    pandas_ta \
    tk \
    matplotlib \
    boto3"

ENV ML_GPU="\
    mlflow \
    protobuf==3.15.0 \
    tensorflow-gpu \
    tensorflow-datasets \
    torch \
    torchvision \
    scipy \
    scikit-learn \
    scikit-image \
    pillow==9.2.0 \
    nibabel \
    opencv-python \
    gym \
    segmentation-models \
    albumentations\
    PyMCubes\
    trimesh"


RUN case "$IMAGE_TYPE" in \
            'GPU') pip install $ML_CPU $ML_GPU  ;; \
            'CPU') pip install $ML_CPU ;; \
    esac;

# Install Jupyter
#

RUN chown -R $USER:$USER /usr/local/share/jupyter
COPY resources/jupyter/themes.jupyterlab-settings /home/$USER/.jupyter/lab/user-settings/@jupyterlab/apputils-extension/themes.jupyterlab-settings
COPY resources/jupyter/shortcuts.jupyterlab-settings /home/$USER/.jupyter/lab/user-settings/@jupyterlab/shortcuts-extension/shortcuts.jupyterlab-settings
COPY resources/jupyter/tracker.jupyterlab-settings /home/$USER/.jupyter/lab/user-settings/@jupyterlab/notebook-extension/tracker.jupyterlab-settings
COPY resources/jupyter/terminal-plugin.jupyterlab-settings /home/$USER/.jupyter/lab/user-settings/@jupyterlab/terminal-extension/plugin.jupyterlab-settings
COPY resources/jupyter/extension-plugin.jupyterlab-settings /home/$USER/.jupyter/lab/user-settings/@jupyterlab/extensionmanager-extension/plugin.jupyterlab-settings

# Install Clojure
#
RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -P /usr/bin/ && \
    chmod 755 /usr/bin/lein && \
    /bin/bash -c "$(curl -fsSL https://download.clojure.org/install/linux-install-1.11.1.1113.sh)" && \
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)" && \
    /bin/bash -c "$(curl -s https://raw.githubusercontent.com/babashka/babashka/master/install)" && \
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo)"

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

RUN mkdir /var/lib/pgadmin && \
    mkdir /var/log/pgadmin && \
    chown $USER /var/lib/pgadmin && \
    chown $USER /var/log/pgadmin && \
    python3 -m venv /opt/pgadmin && \
    /opt/pgadmin/bin/python -m pip install install pgadmin4 && \
    sed -i -e "s/^DEFAULT_SERVER =.*/DEFAULT_SERVER=\'0\.0\.0\.0\'/g" /opt/pgadmin/lib/python3.8/site-packages/pgadmin4/config.py

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
COPY resources/bin/bootrc /home/$USER/.bootrc

RUN mkdir -p /home/$USER/.local/share/ && \
    chown -R $USER:$USER /opt/emacsd && \
    chown -R $USER:$USER /home/$USER && \
    chown -R $USER:$USER /storage

USER $USER
