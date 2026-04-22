FROM php:8.1.10-zts-bullseye

WORKDIR /app

RUN apt update && apt install -y protobuf-compiler wget git
RUN pecl install protobuf

RUN wget https://github.com/spiral/php-grpc/releases/download/v1.6.0/protoc-gen-php-grpc-1.6.0-linux-amd64.tar.gz
RUN wget https://github.com/spiral/php-grpc/releases/download/v1.6.0/rr-grpc-1.6.0-linux-amd64.tar.gz
RUN tar -xvf protoc-gen-php-grpc-1.6.0-linux-amd64.tar.gz && cp protoc-gen-php-grpc-1.6.0-linux-amd64/protoc-gen-php-grpc /usr/bin/
RUN tar -xvf rr-grpc-1.6.0-linux-amd64.tar.gz && cp rr-grpc-1.6.0-linux-amd64/rr-grpc /usr/bin/

COPY proto /app/proto
COPY php_grpc_bench/composer.json /app/composer.json

RUN mkdir src && protoc --php_out=src --php-grpc_out=src --proto_path=/app/proto/helloworld helloworld.proto
RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php composer-setup.php --quiet
RUN ./composer.phar install

COPY php_grpc_bench /app

ENTRYPOINT [ "rr-grpc", "serve" ]
