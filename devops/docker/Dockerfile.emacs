FROM ubuntu:20.04
ENV DISTRO=focal

ENV USER="nakkaya" \
    UID=1000 \
    TZ=Asia/Nicosia

RUN sed -Ei 's/^# deb-src /deb-src /' /etc/apt/sources.list
RUN apt-get update && apt-get upgrade -y

# Set Timezone
#
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    LC_ALL=C.UTF-8 \
    DEBIAN_FRONTEND=noninteractive

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone \
    apt-get install tzdata -y --no-install-recommends \
    dpkg-reconfigure tzdata

# Install Packages
#
RUN apt-get install \
    # apt
    equivs devscripts gnupg software-properties-common \
    # Misc
    openssh-server sudo curl iputils-ping bash-completion \
    unzip wget htop xz-utils \
    graphviz postgresql-client qutebrowser\
    # Backup & Storage
    rclone git git-annex git-annex-remote-rclone \
    apt-transport-https apache2-utils \
    # Java
    openjdk-11-jdk maven  \
    # C/C++
    build-essential clang clangd cmake cppcheck valgrind \
    # Emacs Native Comp Deps
    gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson-dev \
    # Python
    python3 python3-dev python3-pip \
    # Latex
    texlive-latex-base texlive-xetex texlive-lang-english \
    texlive-lang-european texlive-plain-generic pandoc latexmk \
    # PDF Tools
    libpng-dev zlib1g-dev libpoppler-glib-dev \
    libpoppler-private-dev imagemagick \
    # For Teensy
    libxft2 \
    -y --no-install-recommends

RUN apt-get install ispell -y

# SSH
#

RUN service ssh start

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
    apt-get install docker-ce-cli -y --no-install-recommends

# Clean Apt
#
RUN apt-get clean && apt-get autoclean

# Configure Python
#
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 1 && \
    update-alternatives --install /usr/bin/pip pip /usr/bin/pip3 1 && \
    pip install invoke \
    python-lsp-server[all] \
    ansible \
    jupyterlab \
    pyinotify pyxdg paramiko

# Install Clojure
#
RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -P /usr/bin/ && \
    chmod 755 /usr/bin/lein && \
    curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install -o install && \
    bash install && \
    rm install

# Install AWS CLI
#
RUN ARCH="$(dpkg --print-architecture)"; \
    case "$ARCH" in \
            amd64) URL='https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip' ;; \
            arm64) URL='https://awscli.amazonaws.com/awscli-exe-linux-aarch64.zip' ;; \
    esac; \
    curl -s "${URL}" -o "awscliv2.zip" && \
    unzip -q awscliv2.zip && \
    ./aws/install && \
    rm awscliv2.zip && \
    pip install boto3

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

# Setup User
#
COPY resources/conf/bashrc /home/$USER/.bashrc
COPY resources/conf/bash_profile /home/$USER/.bash_profile
ADD resources/media/JetBrainsMono.ttf /usr/local/share/fonts

RUN useradd -u $UID -s /bin/bash $USER && \
    usermod -a -G dialout $USER && \
    adduser $USER sudo && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers && \
    #
    mkdir -p /home/$USER/ && \
    mkdir -p /home/$USER/.config/ && \
    mkdir -p /home/$USER/.cache/ && \
    mkdir /storage && \
    touch /home/$USER/.sudo_as_admin_successful

# Build Emacs
#
RUN mk-build-deps emacs \
    --install \
    --remove \
    --tool='apt-get -o Debug::pkgProblemResolver=yes --no-install-recommends --yes' && \
    git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git /opt/emacsd/src && \
    cd /opt/emacsd/src && \
    ./autogen.sh && \
    CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10 CFLAGS="-O3 -fomit-frame-pointer" ./configure \
    --with-native-compilation \
    --with-modules \
    --with-json \
    --with-mailutils \
    --with-x=yes \
    --with-x-toolkit=gtk3 \
    --with-png=yes && \
    make -j$(nproc) && \
    make install && \
    cd /opt/emacsd/ && \
    rm -rf src && \
    apt-get purge emacs-build-deps -y && \
    apt-get clean && \
    apt-get autoclean

# Install epdfinfo (pdf-tools)
#
ENV PATH="/opt/cask/bin:$PATH"
RUN git clone https://github.com/cask/cask /opt/cask && \
    git clone https://github.com/politza/pdf-tools.git && \
    cd pdf-tools && \
    make -s && \
    sudo mv server/epdfinfo /usr/bin/ && \
    cd ../ && \
    rm -rf pdf-tools

# Install XPRA
#
RUN wget -q https://xpra.org/gpg.asc -O- | apt-key add - && \
    add-apt-repository "deb https://xpra.org/ $DISTRO main" && \
    apt-get update && \
    apt-get install xpra xpra-html5 -y --no-install-recommends && \
    apt-get clean && \
    apt-get autoclean

RUN sed -i -e 's/\(<title>\)[^<]*\(<\/title>\)/\1emacsd\2/g' /usr/share/xpra/www/index.html && \
    sed -i -e 's/\(<title>\)[^<]*\(<\/title>\)/\1emacsd\2/g' /usr/share/xpra/www/connect.html && \
    rm -rf /usr/share/xpra/www/default-settings.txt* && \
    touch /usr/share/xpra/www/default-settings.txt && \
    echo 'keyboard = false' >> /usr/share/xpra/www/default-settings.txt && \
    echo 'floating_menu = false' >> /usr/share/xpra/www/default-settings.txt && \
    #
    mkdir /run/user/$UID && \
    mkdir /run/xpra && \
    chmod 775 /run/xpra && \
    chown -R $USER:$USER /run/xpra && \
    chown -R $USER:$USER /run/user/$UID

# Setup Emacs
#
RUN git clone https://github.com/nakkaya/emacs /opt/emacsd/conf && \
    echo "(setq user-emacs-directory \"/storage/.emacsd/\")" > /home/$USER/.emacs && \
    echo "(setq package-native-compile t)" > /home/$USER/.emacs && \
    echo "(load-file \"/opt/emacsd/conf/init.el\")" >> /home/$USER/.emacs && \
    echo "(load-file \"/opt/emacsd/conf/emacsd.el\")" >> /home/$USER/.emacs && \
    # Init ENV
    mkdir /opt/emacsd/logs && \
    mkdir /opt/emacsd/server

RUN chown -R $USER:$USER /opt/emacsd && \
    chown -R $USER:$USER /home/$USER && \
    chown -R $USER:$USER /storage
USER $USER

# Run
#
COPY resources/bin/ob-tangle.sh /usr/bin/ob-tangle
RUN sudo chmod +x /usr/bin/ob-tangle

COPY resources/bin/edit.sh /usr/bin/edit
RUN sudo chmod +x /usr/bin/edit

COPY resources/bin/exec.sh /opt/emacsd/
RUN sudo chmod +x /opt/emacsd/exec.sh

WORKDIR "/storage"
CMD /opt/emacsd/exec.sh
