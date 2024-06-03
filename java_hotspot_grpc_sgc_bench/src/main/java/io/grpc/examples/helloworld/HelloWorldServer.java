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

package io.grpc.examples.helloworld;

import io.grpc.Server;
import io.grpc.ServerBuilder;
import io.grpc.stub.StreamObserver;
import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.concurrent.Executors;

/**
 * Server that manages startup/shutdown of a {@code Greeter} server.
 */
public class HelloWorldServer {

  private static final Logger logger = Logger.getLogger(HelloWorldServer.class.getName());
  private Server server;

  private void start() throws IOException {
    /* The port on which the server should run */
    var port = 50051;
    var serverBuilder = configureExecutor(ServerBuilder.forPort(port));
    server = serverBuilder.addService(new GreeterImpl()).build().start();
    logger.info("Server started, listening on " + port);
    Runtime.getRuntime().addShutdownHook(new Thread(() -> {
      // Use stderr here since the logger may have been
      // reset by its JVM shutdown hook.
      System.err.println("*** shutting down gRPC server since JVM is shutting down");
      try {
        server.shutdown().awaitTermination(30, TimeUnit.SECONDS);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
      System.err.println("*** server shut down");
    }));
  }

  /**
   * Allow customization of the Executor with two environment variables:
   *
   * <p>
   * <ul>
   * <li>JVM_EXECUTOR_TYPE: direct, workStealing, single, fixed, cached</li>
   * <li>GRPC_SERVER_CPUS: integer value.</li>
   * </ul>
   * </p>
   *
   * The number of Executor Threads will default to the number of
   * availableProcessors(). Only the workStealing and fixed executors will use
   * this value.
   */
  private ServerBuilder<?> configureExecutor(ServerBuilder<?> sb) {
    var threads = System.getenv("GRPC_SERVER_CPUS");
    var i_threads = Runtime.getRuntime().availableProcessors();
    if (threads != null && !threads.isEmpty()) {
      i_threads = Integer.parseInt(threads);
    }

    var value = System.getenv().getOrDefault("JVM_EXECUTOR_TYPE", "workStealing");
    switch (value) {
      case "direct" -> sb = sb.directExecutor();
      case "single" -> sb = sb.executor(Executors.newSingleThreadExecutor());
      case "fixed" -> sb = sb.executor(Executors.newFixedThreadPool(i_threads));
      case "workStealing" -> sb = sb.executor(Executors.newWorkStealingPool(i_threads));
      case "cached" -> sb = sb.executor(Executors.newCachedThreadPool());
    }

    return sb;
  }

  /**
   * Await termination on the main thread since the grpc library uses daemon
   * threads.
   */
  private void blockUntilShutdown() throws InterruptedException {
    if (server != null) {
      server.awaitTermination();
    }
  }

  /**
   * Main launches the server from the command line.
   */
  public static void main(String[] args) throws IOException, InterruptedException {
    var serverApp = new HelloWorldServer();
    serverApp.start();
    serverApp.blockUntilShutdown();
  }

  static class GreeterImpl extends GreeterGrpc.GreeterImplBase {

    @Override
    public void sayHello(HelloRequest req, StreamObserver<HelloReply> responseObserver) {
      final var reply = HelloReply.newBuilder().setResponse(req.getRequest()).build();
      responseObserver.onNext(reply);
      responseObserver.onCompleted();
    }
  }
}
