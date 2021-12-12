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
                  libncurses-dev \
                  libotf-dev \
                  libotf0 \
                  libgccjit0 \
                  libgccjit-10-dev \
                  libjansson-dev \
                  gnutls-dev"


echo $EMACS_BUILD_DEPS
echo $EMACS_BUILD_TOOLS

sudo apt-get install \
     $EMACS_BUILD_TOOLS \
     $EMACS_BUILD_DEPS \
     -y --no-install-recommends

cd emacs

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
