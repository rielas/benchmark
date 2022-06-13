FROM --platform=linux/amd64 ubuntu:latest as build_image

WORKDIR /home/benchmark

RUN apt-get update && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    software-properties-common
RUN sh -c 'curl -sSL https://get.haskellstack.org/ | sh'

COPY stack.yaml .
COPY package.yaml .

RUN stack build
