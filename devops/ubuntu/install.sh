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
		   libgtk-3-dev \
		   libotf-dev \
		   libharfbuzz-dev \
		   libjansson-dev \
		   libwebkit2gtk-4.0-dev \
		   libgccjit-10-dev \
		   libgif-dev \
		   libxpm-dev \
		   gnutls-dev \
                   libncurses-dev"

EMACS_BUILD_DEPS="libgtk-3-0 \
		  libharfbuzz-bin \
		  libwebkit2gtk-4.0 \
		  libotf-bin \
		  libgccjit0 \
		  libjansson4 \
		  libm17n-0 \
		  libgccjit0"

sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install $EMACS_BUILD_TOOLS $EMACS_BUILD_DEPS -y --no-install-recommends

rm -f ~/.emacs
echo "(load-file \"`pwd`/../../init.el\")" > ~/.emacs

if [ -d ~/.emacs.build ]; then rm -Rf ~/.emacs.build; fi

git clone --depth 1 --branch emacs-28.2 https://git.savannah.gnu.org/git/emacs.git ~/.emacs.build

cd ~/.emacs.build

export CC=/usr/bin/gcc-10
export CXX=/usr/bin/gcc-10
export CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"

./autogen.sh
./configure \
    --with-zlib \
    --with-native-compilation \
    --with-modules \
    --with-json \
    --with-mailutils \
    --with-xml2 \
    --with-xft \
    --with-libotf \
    --with-gnutls=yes \
    --with-x=yes \
    --with-cairo \
    --with-xwidgets \
    --with-x-toolkit=gtk3 \
    --with-harfbuzz \
    --with-jpeg=yes \
    --with-png=yes
make -j$(nproc)

mkdir -p ~/.local/share/applications

if [ -f ~/.local/share/applications/emacs28.desktop ];
then
    rm ~/.local/share/applications/emacs28.desktop;
fi

echo "#!/usr/bin/env xdg-open" > ~/.local/share/applications/emacs28.desktop
echo "[Desktop Entry]" >> ~/.local/share/applications/emacs28.desktop
echo "Name=Emacs 28" >> ~/.local/share/applications/emacs28.desktop
echo "Icon=/usr/share/icons/hicolor/scalable/apps/emacs.svg" >> ~/.local/share/applications/emacs28.desktop
echo "Exec=$HOME/.emacs.build/src/emacs" >> ~/.local/share/applications/emacs28.desktop
echo "Type=Application" >> ~/.local/share/applications/emacs28.desktop
echo "Terminal=false" >> ~/.local/share/applications/emacs28.desktop
echo "StartupNotify=true" >> ~/.local/share/applications/emacs28.desktop

#sudo pip3 install jupyterlab

sudo adduser $USER dialout
sudo adduser $USER dialout

# https://askubuntu.com/a/223674
# gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps']"
