FROM nakkaya/env:latest

# Get Emacs Build Deps
#
RUN apt-get build-dep emacs-nox -y
RUN apt-get install gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson-dev -y
RUN apt-get clean && apt-get autoclean

ADD resources/media/JetBrainsMono.ttf /usr/local/share/fonts

# Build Emacs
#
RUN git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git /opt/emacsd/src
WORKDIR /opt/emacsd/src
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
run rm -rf /opt/emacsd/src

# Build GoTTY
#
RUN git clone --depth 1 https://github.com/sorenisanerd/gotty.git /opt/gotty
WORKDIR /opt/gotty
ADD resources/media/icon.svg /opt/gotty/resources/icon.svg
ADD resources/media/icon_192.png /opt/gotty/resources/icon_192.png
ADD resources/media/favicon.ico /opt/gotty/resources/favicon.ico
RUN make gotty
RUN mv /opt/gotty/gotty /usr/bin/gotty
WORKDIR /
RUN rm -rf /opt/gotty
ADD resources/conf/gotty /home/$USER/.gotty

# Install XPRA
#
RUN wget -q https://xpra.org/gpg.asc -O- | apt-key add -
RUN add-apt-repository "deb https://xpra.org/ $DISTRO main"
RUN apt-get update
RUN apt-get install \
    xpra xpra-html5 \
    -y --no-install-recommends
RUN apt-get clean && apt-get autoclean

RUN sed -i -e 's/\(<title>\)[^<]*\(<\/title>\)/\1emacsd\2/g' /usr/share/xpra/www/index.html
RUN rm -rf /usr/share/xpra/www/default-settings.txt*
RUN touch /usr/share/xpra/www/default-settings.txt
RUN echo 'keyboard = false' >> /usr/share/xpra/www/default-settings.txt
RUN echo 'floating_menu = false' >> /usr/share/xpra/www/default-settings.txt

RUN pip3 install pyinotify pyxdg paramiko
RUN mkdir /run/user/$UID
RUN mkdir /run/xpra
RUN chmod 775 /run/xpra
RUN chown -R $USER:$USER /run/xpra
RUN chown -R $USER:$USER /run/user/$UID

# Copy Settings
#
RUN git clone https://github.com/nakkaya/emacs /opt/emacsd/conf
RUN echo "(setq package-native-compile t)" > /home/$USER/.emacs
RUN echo "(load-file \"/opt/emacsd/conf/init.el\")" >> /home/$USER/.emacs
RUN echo "(load-file \"/opt/emacsd/conf/emacsd.el\")" >> /home/$USER/.emacs

# Init ENV
#
RUN mkdir /opt/emacsd/logs
RUN mkdir /opt/emacsd/server
RUN chown -R $USER:$USER /opt/emacsd
RUN chown -R $USER:$USER /home/$USER
USER $USER

# Install epdfinfo
#
WORKDIR /home/$USER/
RUN curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python3
ENV PATH="/home/$USER/.cask/bin:$PATH"
RUN git clone https://github.com/politza/pdf-tools.git
WORKDIR "pdf-tools"
RUN make -s
RUN sudo mv server/epdfinfo /usr/bin/
WORKDIR /home/$USER/
RUN rm -rf pdf-tools

# AOT Compile Emacs Packages
#
RUN emacs --batch -l /home/$USER/.emacs

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
