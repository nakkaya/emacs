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
    rsync rclone git git-annex git-annex-remote-rclone \
    apt-transport-https apache2-utils \
    # Java
    openjdk-11-jdk maven visualvm  \
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


RUN python3 -m venv /opt/skypilot && \
    /opt/skypilot/bin/python -m pip install wheel "colorama==0.4.4" "click==8.0.3" --no-cache-dir && \
    /opt/skypilot/bin/python -m pip install "ray==2.2.0" --no-cache-dir && \
    /opt/skypilot/bin/python -m pip install "skypilot==0.2.2" --no-cache-dir && \
    echo "export PATH=\"\$PATH:/opt/skypilot/bin\"" >> /home/$USER/.bashrc

RUN python3 -m venv /opt/ansible && \
    /opt/ansible/bin/python -m pip install ansible ansible-lint --no-cache-dir && \
    echo "export PATH=\"\$PATH:/opt/ansible/bin\"" >> /home/$USER/.bashrc

RUN python3 -m venv /opt/invoke && \
    /opt/invoke/bin/python -m pip install invoke --no-cache-dir && \
    echo "export PATH=\"\$PATH:/opt/invoke/bin\"" >> /home/$USER/.bashrc

ENV ML_CPU="\
    requests==2.28.1 \
    jinja2==3.1.2 \
    jupyterlab==3.5.2 \
    ipywidgets==8.0.4 \
    numpy==1.23.5 \
    numexpr==2.8.4 \
    pandas==1.5.2 \
    pandas_ta==0.3.14b \
    matplotlib==3.6.2 \
    boto3==1.26.45"

RUN pip install $ML_CPU --no-cache-dir

RUN if [ "$IMAGE_TYPE" = "GPU" ]; \
    then \
    wget -q https://developer.download.nvidia.com/compute/cuda/11.8.0/local_installers/cuda_11.8.0_520.61.05_linux.run && \
    sh cuda_11.8.0_520.61.05_linux.run --silent --toolkit && \
    rm cuda_11.8.0_520.61.05_linux.run && \
    add-apt-repository -y ppa:flexiondotorg/nvtop && apt-get install nvtop -y --no-install-recommends \
    ; fi

ENV ML_GPU="\
    urllib3==1.26.13 \
    mlflow==2.1.1 \
    protobuf==3.19.6 \
    tensorflow-gpu==2.11.0 \
    tensorflow-datasets==4.8.0 \
    torch==1.13.1 \
    torchvision==0.14.1 \
    scipy==1.9.3 \
    scikit-learn==1.2.0 \
    scikit-image==0.19.3 \
    pillow==9.4.0 \
    nibabel==4.0.2 \
    opencv-python==4.7.0.68 \
    segmentation-models==1.0.1 \
    albumentations==1.3.0 \
    PyMCubes==0.1.2 \
    trimesh==3.17.1"

RUN if [ "$IMAGE_TYPE" = "GPU" ]; \
    then \
    pip install $ML_GPU --no-cache-dir \
    ; fi

# Install Miniconda
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://repo.anaconda.com/miniconda/Miniconda3-py38_22.11.1-1-Linux-x86_64.sh' ;; \
            arm64) URL='https://repo.anaconda.com/miniconda/Miniconda3-py38_22.11.1-1-Linux-aarch64.sh' ;; \
    esac; \
    curl -s "${URL}" -o "Miniconda3.sh" && \
    bash Miniconda3.sh -b -u -p /opt/conda && \
    su - $USER -c '/opt/conda/bin/conda init bash' && \
    rm -rf Miniconda3.sh

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
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)" -- --static && \
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
    /opt/pgadmin/bin/python -m pip install install wheel --no-cache-dir && \
    /opt/pgadmin/bin/python -m pip install install pgadmin4 --no-cache-dir && \
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
