FROM ubuntu:22.04

ARG PROXY=""

ARG GID=1000
ARG UID=1000
ARG UNAME=lctags

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

RUN if grep :$GID: /etc/group; then \
		echo exist; else groupadd -g $GID $UNAME; fi
RUN if grep :$UID: /etc/passwd; then \
		echo exist; else useradd -g $GID -u $UID $UNAME; fi
RUN apt-get install -y sudo
RUN echo "$UNAME ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers

WORKDIR /opt/
RUN git clone --depth 1 https://github.com/ifritJP/lctags.git
WORKDIR /opt/lctags/src
RUN make build_for_apt
RUN make install
