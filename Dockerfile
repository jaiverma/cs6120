FROM ubuntu:focal
MAINTAINER jaiverma <jai2.verma@outlook.com>

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y \
    && apt-get install nodejs -y \
    && apt-get install npm -y \
    && apt-get install yarnpkg -y \
    && apt-get install python3 -y \
    && apt-get install python3-pip -y \
    && apt-get install git -y

RUN ln -s /usr/bin/python3.8 /usr/bin/python \
    && ln -s /usr/bin/pip3 /usr/bin/pip \
    && ln -s /usr/bin/yarnpkg /usr/bin/yarn

WORKDIR /root

RUN git clone https://github.com/sampsyo/bril

# install typescript compiler for Bril
WORKDIR /root/bril/bril-ts
RUN yarn \
    && yarn build \
    && yarn link

# install bril2json and bril2text
WORKDIR /root/bril/bril-txt
ENV FLIT_ROOT_INSTALL=1
RUN pip install flit \
    && flit install \
    && cp /root/.local/bin/bril2json /usr/local/bin \
    && cp /root/.local/bin/bril2txt /usr/local/bin \
    && pip install turnt

WORKDIR /mnt/host
