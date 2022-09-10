FROM php:zts-buster

WORKDIR /app

RUN apt update && apt install -y protobuf-compiler wget git
RUN pecl install protobuf

RUN wget https://github.com/roadrunner-server/roadrunner/releases/download/v2.11.1/protoc-gen-php-grpc-2.11.1-linux-amd64.tar.gz
RUN wget https://github.com/roadrunner-server/roadrunner/releases/download/v2.11.1/roadrunner-2.11.1-linux-amd64.tar.gz
RUN tar -xvf protoc-gen-php-grpc-2.11.1-linux-amd64.tar.gz && cp protoc-gen-php-grpc-2.11.1-linux-amd64/protoc-gen-php-grpc /usr/bin/
RUN tar -xvf roadrunner-2.11.1-linux-amd64.tar.gz && cp roadrunner-2.11.1-linux-amd64/rr /usr/bin/

COPY proto /app/proto
COPY php_grpc_bench/composer.json /app/composer.json

RUN mkdir src && protoc --php_out=src --php-grpc_out=src --proto_path=/app/proto/helloworld helloworld.proto
RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php composer-setup.php --quiet
RUN ./composer.phar install

COPY php_grpc_bench /app

ENTRYPOINT [ "rr", "serve" ]
