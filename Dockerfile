FROM ubuntu:latest as builder

WORKDIR /home/benchmark

RUN apt-get update && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    software-properties-common
RUN sh -c 'curl -sSL https://get.haskellstack.org/ | sh'

COPY stack.yaml .
COPY package.yaml .
RUN stack setup

COPY src src
COPY app app
COPY test test
COPY README.md .
COPY ChangeLog.md .

RUN stack build --copy-bins

FROM ubuntu:latest

COPY --from=builder /root/.local/bin/benchmark-exe /usr/bin/benchmark
