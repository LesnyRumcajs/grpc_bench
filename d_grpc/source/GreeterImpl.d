module GreeterImpl;

import helloworld.helloworld;
import helloworld.helloworldrpc;
import grpc;

/**
 *
 */
class GreeterImpl : GreeterBase {
    override Status SayHello(HelloRequest request, ref HelloReply reply) {
        reply.message = request.name;
        return Status.OK;
    }
}
