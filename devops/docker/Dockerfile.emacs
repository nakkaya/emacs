FROM nakkaya/env:latest
ARG USER="nakkaya"
WORKDIR "/"

# Get Emacs Build Deps
#
RUN DEBIAN_FRONTEND=noninteractive apt-get build-dep emacs-nox -y
RUN apt-get install gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson-dev -y
RUN apt-get clean && apt-get autoclean

ADD resources/JetBrainsMono.ttf /usr/local/share/fonts

# Build Emacs
#
WORKDIR "/opt/"
# --branch emacs-27
RUN git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git
WORKDIR "/opt/emacs"
RUN ./autogen.sh
# Check system-configuration-options for options
RUN CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10 CFLAGS="-O3 -fomit-frame-pointer" ./configure \
    --with-native-compilation \
    --with-modules \
    --with-json \
    --with-mailutils \
    --with-x=yes \
    --with-x-toolkit=gtk3 \
    --with-png=yes
RUN make -j$(nproc)
RUN make install

# Build GoTTY
#
WORKDIR "/"
RUN git clone https://github.com/sorenisanerd/gotty.git /opt/gotty
WORKDIR "/opt/gotty"
ADD resources/icon.svg /opt/gotty/resources/icon.svg
ADD resources/icon_192.png /opt/gotty/resources/icon_192.png
ADD resources/favicon.ico /opt/gotty/resources/favicon.ico
RUN make gotty
WORKDIR "/"
RUN ln -s /opt/gotty/gotty /usr/bin/gotty
ADD resources/gotty /home/$USER/.gotty

# Install XPRA
#
ENV DISTRO=focal
#install https support for apt (which may be installed already):
RUN apt-get update
RUN apt-get install apt-transport-https
# add Xpra GPG key
RUN wget -q https://xpra.org/gpg.asc -O- | apt-key add -
# add Xpra repository
RUN add-apt-repository "deb https://xpra.org/ $DISTRO main"
# install Xpra package
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install xpra xpra-html5 -y --no-install-recommends
RUN apt-get clean && apt-get autoclean

# Copy Settings
#
COPY resources/exec.sh /usr/bin/
RUN chmod 755 /usr/bin/exec.sh
RUN git clone https://github.com/nakkaya/emacs /opt/emacsd/emacs
RUN echo "(setq package-native-compile t)" > /home/$USER/.emacs
RUN echo "(load-file \"/opt/emacsd/emacs/init.el\")" >> /home/$USER/.emacs
RUN echo "(load-file \"/opt/emacsd/emacs/pdf-tools.el\")" >> /home/$USER/.emacs

# Init ENV
#
RUN chown -R $USER:$USER /opt/emacsd
RUN chown -R $USER:$USER /home/$USER
ENV EMACS_HOME_DIR=/storage/ \
    TERM=xterm-256color

USER $USER
RUN emacs --batch -l /opt/emacsd/emacs/init.el
WORKDIR "/storage"
CMD ["exec.sh"]
