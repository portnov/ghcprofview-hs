#!/bin/bash
set -e
set -x

TARGET=ghcprofview_0.1.0.0.orig.tar.xz 
tar -v -cJ --exclude-vcs --exclude='*.cabal' --exclude='*.log' --exclude='*.build' --exclude=./docker --exclude=./work --exclude=./.stack-work --exclude=./debian --exclude=./test --exclude='*.tar.xz' -f ../$TARGET .

LOCAL_INSTALL_ROOT=$(stack path --local-install-root --allow-different-user)
LOCAL_INSTALL_ROOT=$(realpath --relative-to=$PWD $LOCAL_INSTALL_ROOT)
echo "$LOCAL_INSTALL_ROOT/bin/ghcprofview usr/bin" > debian/ghcprofview.install

debuild -uc -us

