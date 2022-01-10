<?php

use Grpc\Parser;
use Helloworld\HelloReply;
use Helloworld\HelloRequest;

require __DIR__ . '/vendor/autoload.php';

$http = new Swoole\Http\Server('0.0.0.0', 50051, SWOOLE_BASE);
$http->set([
    'trace_flags' => 0,
    'log_file' => '/dev/null',
    'log_level' => 5,
    'open_http2_protocol' => true
]);

$http->on('request', function ($request, $response) {

    try {
        $request_message = new HelloRequest;

        $response_message = new HelloReply();
        $response_message->setResponse($request_message->getRequest());

        $response->header('content-type', 'application/grpc');
        $response->header('trailer', 'grpc-status, grpc-message');

        $response->trailer('grpc-status' , '0');
        $response->trailer('grpc-message', '');

        $response->end(Parser::pack($response_message->serializeToString()));
    } catch (Throwable $e) {
        $response->status(400);
        $response->end('Bad Request');
    }
});

$http->start();
