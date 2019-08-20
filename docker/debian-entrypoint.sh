#!/bin/bash
set -e
set -x

cd /src
bash prepare_debian_package.sh

cp ../*.deb /dst/
