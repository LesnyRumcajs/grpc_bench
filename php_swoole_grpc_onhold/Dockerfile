FROM phpswoole/swoole:php8.1-alpine

WORKDIR /app

COPY php_swoole_grpc_bench/composer.json /app/composer.json
COPY php_swoole_grpc_bench/server.php /app/server.php

RUN composer install --optimize-autoloader --classmap-authoritative

CMD php server.php
