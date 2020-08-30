FROM ubuntu

RUN apt-get -q update \
    && apt-get -q -y install wget \
    && wget https://github.com/bojand/ghz/releases/download/v0.58.0/ghz-linux-x86_64.tar.gz \
    && tar zxf ghz-linux-x86_64.tar.gz -C /usr/bin \
    && apt-get -q -y remove wget \
    && apt-get -q -y autoremove \
    && rm -rf /var/cache/apt/archives

ENTRYPOINT [ "/usr/bin/ghz" ]
