#!/usr/bin/env bash

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
                   g++-10"

EMACS_BUILD_DEPS="xaw3dg \
                  xaw3dg-dev \
		  libxaw7-dev \
                  libncurses-dev \
                  libotf-dev \
                  libotf0 \
                  libgccjit0 \
                  libgccjit-10-dev \
                  libjansson-dev \
                  gnutls-dev"

sudo apt-get install \
     $EMACS_BUILD_TOOLS \
     $EMACS_BUILD_DEPS \
     -y --no-install-recommends

if [ -d ~/.emacs.build ]; then rm -Rf ~/.emacs.build; fi

git clone --depth 1 --branch emacs-28 https://git.savannah.gnu.org/git/emacs.git ~/.emacs.build

cd ~/.emacs.build

export CC=/usr/bin/gcc-10
export CXX=/usr/bin/gcc-10
export CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"

./autogen.sh
./configure \
    --without-all \
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
    --with-x-toolkit=lucid \
    --with-png=yes && \
    make -j$(nproc)

if [ -f ~/.local/share/applications/emacs28.desktop ];
then
    rm ~/.local/share/applications/emacs28.desktop;
fi

echo "#!/usr/bin/env xdg-open" > ~/.local/share/applications/emacs28.desktop
echo "[Desktop Entry]" >> ~/.local/share/applications/emacs28.desktop
echo "Name=Emacs 28" >> ~/.local/share/applications/emacs28.desktop
echo "Icon=/usr/share/icons/hicolor/scalable/apps/emacs.svg" >> ~/.local/share/applications/emacs28.desktop
echo "Exec=~/.emacs.build/src/emacs" >> ~/.local/share/applications/emacs28.desktop
echo "Type=Application" >> ~/.local/share/applications/emacs28.desktop
echo "Terminal=false" >> ~/.local/share/applications/emacs28.desktop
echo "StartupNotify=true" >> ~/.local/share/applications/emacs28.desktop
