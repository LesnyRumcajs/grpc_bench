/*
 * Copyright 2015 The gRPC Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.linecorp.armeria.examples.helloworld;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Logger;

import com.linecorp.armeria.common.grpc.GrpcSerializationFormats;
import com.linecorp.armeria.server.Server;
import com.linecorp.armeria.server.grpc.GrpcService;

import io.grpc.examples.helloworld.GreeterGrpc;
import io.grpc.examples.helloworld.HelloReply;
import io.grpc.examples.helloworld.HelloRequest;
import io.grpc.stub.StreamObserver;

/**
 * Server that manages startup/shutdown of a {@code Greeter} server.
 */
public final class HelloWorldServer {

    private static final Logger logger = Logger.getLogger(HelloWorldServer.class.getName());

    private static void start() throws IOException {
        /* The port on which the server should run */
        final var port = 50051;
        final var server = Server.builder()
                                 .http(port)
                                 .service(GrpcService.builder()
                                                     .addService(new GreeterImpl())
                                                     .supportedSerializationFormats(
                                                             GrpcSerializationFormats.PROTO)
                                                     .build())
                                 .disableDateHeader()
                                 .disableServerHeader()
                                 .build();
        server.start().join();
        logger.info("Server started, listening on " + port);
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            // Use stderr here since the logger may have been
            // reset by its JVM shutdown hook.
            System.err.println("*** shutting down gRPC server since JVM is shutting down");
            try {
                server.stop().get(30, TimeUnit.SECONDS);
            } catch (InterruptedException | ExecutionException | TimeoutException e) {
                e.printStackTrace();
            }
            System.err.println("*** server shut down");
        }));
    }

    /**
     * Main launches the server from the command line.
     */
    public static void main(String[] args) throws IOException, InterruptedException {
        start();
    }

    private static final class GreeterImpl extends GreeterGrpc.GreeterImplBase {
        @Override
        public void sayHello(HelloRequest req, StreamObserver<HelloReply> responseObserver) {
            final var reply = HelloReply.newBuilder().setResponse(req.getRequest()).build();
            responseObserver.onNext(reply);
            responseObserver.onCompleted();
        }
    }
}
