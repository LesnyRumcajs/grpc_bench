module GreeterImpl;

import helloworld.helloworld;
import helloworld.helloworldRpc;
import grpc;

/**
 *
 */
class GreeterImpl : GreeterBase {
    override Status SayHello(HelloRequest request, ref HelloReply reply) {
        reply.response = request.request;
        return Status.OK;
    }
}
