package vertx;

import io.grpc.examples.helloworld.GreeterGrpc;
import io.grpc.examples.helloworld.Hello;
import io.grpc.examples.helloworld.HelloReply;
import io.grpc.examples.helloworld.HelloRequest;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpServer;
import io.vertx.grpc.server.GrpcServer;
import io.vertx.grpc.server.GrpcServerResponse;

public class Main {
    public static void main(String[] args) {
        Vertx vertx = Vertx.vertx();
        DeploymentOptions deploymentOptions = new DeploymentOptions();
        int cores = Runtime.getRuntime().availableProcessors();
        String GRPC_SERVER_CPUS = System.getenv("GRPC_SERVER_CPUS");
        if (GRPC_SERVER_CPUS != null && GRPC_SERVER_CPUS.length() > 0) {
            cores = Integer.parseInt(GRPC_SERVER_CPUS);
        }
        deploymentOptions.setInstances(cores);
        vertx.deployVerticle(ServerVerticle::new, deploymentOptions);
        System.out.println("Deploy vertical instances:"+cores);
    }

    static class ServerVerticle extends AbstractVerticle {
        @Override
        public void start(Promise<Void> startPromise) throws Exception {
            GrpcServer grpcServer = GrpcServer.server(vertx);
            grpcServer.callHandler(GreeterGrpc.getSayHelloMethod(), request -> {
                request.handler(helloRequest -> {
                    GrpcServerResponse<HelloRequest, HelloReply> response = request.response();
                    Hello hello = helloRequest.getRequest();
                    HelloReply reply = HelloReply.newBuilder().setResponse(hello).build();
                    response.end(reply);
                });
            });
            HttpServer httpServer = vertx.createHttpServer();
            httpServer.requestHandler(grpcServer).listen(50051).andThen(u -> {
                System.out.println("deployed: " + deploymentID());
                startPromise.complete();
            });
        }
    }
}
