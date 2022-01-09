package helloworld;

// tag::imports[]
import io.grpc.examples.helloworld.GreeterGrpc;
import io.grpc.examples.helloworld.HelloReply;
import io.grpc.examples.helloworld.HelloRequest;
import io.grpc.stub.StreamObserver;
import javax.inject.Singleton;
// end::imports[]

// tag::clazz[]
@Singleton
public class GreetingEndpoint extends GreeterGrpc.GreeterImplBase { // <1>

    private final GreetingService greetingService;

    // <2>
    public GreetingEndpoint(GreetingService greetingService) {
        this.greetingService = greetingService;
    }

    @Override
    public void sayHello(HelloRequest request, StreamObserver<HelloReply> responseObserver) {
        // <3>
        final var reply = HelloReply.newBuilder().setResponse(request.getRequest()).build();
        responseObserver.onNext(reply);
        responseObserver.onCompleted();
    }
}
// end::clazz[]
