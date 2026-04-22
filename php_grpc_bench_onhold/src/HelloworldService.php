<?php
/**
 * Sample GRPC PHP server.
 */

use Spiral\GRPC\ContextInterface;
use Helloworld\GreeterInterface;
use Helloworld\HelloRequest;
use Helloworld\HelloReply;

class HelloworldService implements GreeterInterface
{
    public function SayHello(ContextInterface $ctx, HelloRequest $in): HelloReply
    {
        $out = new HelloReply();
        return $out->setResponse($in->getRequest());
    }
}
