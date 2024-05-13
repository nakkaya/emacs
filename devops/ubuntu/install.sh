#!/usr/bin/env bash

set -e

EMACS_BUILD_TOOLS="wget \
                   gnupg \
                   software-properties-common \
                   equivs \
                   devscripts \
                   autoconf \
                   make \
                   pkg-config \
                   texinfo \
                   gcc-10 \
                   g++-10 \
                   libgccjit-10-dev \
                   libsqlite3-dev \
                   libxpm-dev \
                   libjpeg-dev \
                   libgif-dev \
                   libtiff-dev \
                   libgnutls28-dev \
                   libjansson-dev \
                   libncurses5-dev"

EMACS_BUILD_DEPS="libx11-dev \
                  libgtk-3-dev \
                  libgccjit0 \
                  libjansson4 \
                  libm17n-0 \
                  libgif7 \
                  libotf1 \
                  libsqlite3-0"

sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install $EMACS_BUILD_TOOLS $EMACS_BUILD_DEPS -y --no-install-recommends

rm -f ~/.emacs
echo "(load-file \"`pwd`/../../init.el\")" > ~/.emacs

if [ -d ~/.emacs.build ]; then rm -Rf ~/.emacs.build; fi

git clone --depth 1 --branch emacs-29.1 https://git.savannah.gnu.org/git/emacs.git ~/.emacs.build

cd ~/.emacs.build

export CC=/usr/bin/gcc-10
export CXX=/usr/bin/gcc-10
export CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"

./autogen.sh
./configure \
    --without-sound \
    --with-zlib \
    --with-native-compilation \
    --with-modules \
    --with-json \
    --with-mailutils \
    --with-xml2 \
    --with-sqlite3=yes \
    --with-xft \
    --with-libotf \
    --with-gnutls=yes \
    --with-x=yes \
    --with-cairo \
    --with-x-toolkit=gtk3 \
    --with-harfbuzz \
    --with-jpeg=yes \
    --with-png=yes
make -j$(nproc)

mkdir -p ~/.local/share/applications

if [ -f ~/.local/share/applications/emacs29.desktop ];
then
    rm ~/.local/share/applications/emacs29.desktop;
fi

echo "#!/usr/bin/env xdg-open" > ~/.local/share/applications/emacs29.desktop
echo "[Desktop Entry]" >> ~/.local/share/applications/emacs29.desktop
echo "Name=Emacs 29" >> ~/.local/share/applications/emacs29.desktop
echo "Icon=/usr/share/icons/hicolor/scalable/apps/emacs.svg" >> ~/.local/share/applications/emacs29.desktop
echo "Exec=$HOME/.emacs.build/src/emacs" >> ~/.local/share/applications/emacs29.desktop
echo "Type=Application" >> ~/.local/share/applications/emacs29.desktop
echo "Terminal=false" >> ~/.local/share/applications/emacs29.desktop
echo "StartupNotify=true" >> ~/.local/share/applications/emacs29.desktop

#sudo pip3 install jupyterlab

sudo adduser $USER dialout
sudo adduser $USER dialout

# https://askubuntu.com/a/223674
# gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps']"
