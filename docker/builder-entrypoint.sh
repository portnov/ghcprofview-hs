#!/bin/bash
set -e

curl -sSL https://github.com/portnov/ghcprofview-hs/archive/master.zip -o master.zip
unzip master.zip && rm master.zip
cd ghcprofview-hs-master/
stack install --work-dir=./build --allow-different-user

cp /root/.local/bin/ghcprofview /dst/
