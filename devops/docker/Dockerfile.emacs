FROM nakkaya/env:latest
ARG USER="nakkaya"
WORKDIR "/"

# Get Emacs Build Deps
#
RUN DEBIAN_FRONTEND=noninteractive apt-get build-dep emacs-nox -y
RUN apt-get install gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson-dev -y
RUN apt-get clean && apt-get autoclean

# Build Emacs
#
WORKDIR "/opt/"
# --branch emacs-27
RUN git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git
WORKDIR "/opt/emacs"
RUN ./autogen.sh
RUN CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10 ./configure --with-modules --with-native-compilation --with-json --with-x-toolkit=no --with-xpm=no --with-jpeg=no --with-png=no --with-gif=no --with-tiff=no
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

# Copy Settings
#
COPY resources/exec.sh /usr/bin/
RUN chmod 755 /usr/bin/exec.sh
RUN git clone https://github.com/nakkaya/emacs /opt/emacsd/emacs
RUN echo "(setq package-native-compile t)" > /home/$USER/.emacs
RUN echo "(load-file \"/opt/emacsd/emacs/init.el\")" >> /home/$USER/.emacs

# Init ENV
#
RUN chown -R $USER:$USER /opt/emacsd
RUN chown -R $USER:$USER /home/$USER

USER $USER
RUN emacs --batch --eval '(load "/opt/emacsd/emacs/init.el")'
WORKDIR "/storage"
CMD ["exec.sh"]
