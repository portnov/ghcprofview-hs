#!/bin/bash
set -e

SRC=$(realpath $PWD/..)

docker build -t ghcprofview-ubuntu -f Dockerfile.ubuntu .

docker run --name ghcprofview-ubuntu --rm -v $(pwd)/target:/dst -v $(pwd)/stack:/root/.stack -v $SRC:/src ghcprofview-ubuntu

