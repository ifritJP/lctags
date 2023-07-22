# libclang 9 より新しいと swig が解析エラーするため
# libclang 9 をサポートする中で最新の 20.04 を使用。
FROM ubuntu:20.04

ARG PROXY=""

ARG GID=1000
ARG UID=1000
ARG UNAME=hoge

# apt-get で入力待ちにならないようにするためセット
ENV DEBIAN_FRONTEND=noninteractive
# proxy 設定
ENV HTTP_PROXY=$PROXY
ENV http_proxy=$PROXY
ENV https_proxy=$PROXY
ENV HTTPS_PROXY=$PROXY
RUN if [ "$PROXY" != "" ]; then						\
	echo set $PROXY;						\
	echo "Acquire::http::Proxy \"$PROXY\";" >> /etc/apt/apt.conf;	\
	echo "Acquire::https::Proxy \"$PROXY\";" >> /etc/apt/apt.conf;	\
fi
RUN apt update
RUN apt-get install -y \
	git \
	make \
	diffutils \
	gcc \
	curl \
	libreadline-dev \
	rsync \
	time \
	software-properties-common
RUN apt-get install -y unzip
RUN if [ "$PROXY" != "" ]; then			\
    git config --global http.proxy $PROXY;	\
fi
RUN apt update

RUN if ! cat /etc/group | grep :$GID: ; then \
		echo exist; else groupadd -g $GID $UNAME; fi
RUN if ! cat /etc/passwd | grep :$UID: ; then \
		echo exist; else useradd -g $GID -u $UID $UNAME; fi
RUN apt-get install -y sudo
RUN echo "$UNAME ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers

WORKDIR /opt/
RUN git clone --depth 1 https://github.com/ifritJP/lctags.git
WORKDIR /opt/lctags/src
RUN make build_for_apt
RUN make install

