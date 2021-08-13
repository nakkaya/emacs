FROM nakkaya/env:latest

# Get Emacs Build Deps
#
RUN apt-get build-dep emacs -y && \
    apt-get clean && \
    apt-get autoclean

# Build Emacs
#
RUN git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git /opt/emacsd/src && \
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
    rm -rf src

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
    apt-get clean && apt-get autoclean

RUN sed -i -e 's/\(<title>\)[^<]*\(<\/title>\)/\1emacsd\2/g' /usr/share/xpra/www/index.html && \
    rm -rf /usr/share/xpra/www/default-settings.txt* && \
    touch /usr/share/xpra/www/default-settings.txt && \
    echo 'keyboard = false' >> /usr/share/xpra/www/default-settings.txt && \
    echo 'floating_menu = false' >> /usr/share/xpra/www/default-settings.txt && \
    #
    pip3 install pyinotify pyxdg paramiko && \
    mkdir /run/user/$UID && \
    mkdir /run/xpra && \
    chmod 775 /run/xpra && \
    chown -R $USER:$USER /run/xpra && \
    chown -R $USER:$USER /run/user/$UID

# Setup Emacs
#
RUN git clone https://github.com/nakkaya/emacs /opt/emacsd/conf && \
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
