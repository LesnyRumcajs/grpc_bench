/*
 * Copyright 2020 gRPC authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.grpc.examples.helloworld

import io.grpc.Server
import io.grpc.ServerBuilder

import java.io.IOException
import java.util.concurrent.TimeUnit
import java.util.logging.Logger
import java.util.concurrent.Executors

class HelloWorldServer(val port: Int) {
    val server: Server

    init {
        val threads = System.getenv("JVM_EXECUTOR_THREADS")
        var i_threads = Runtime.getRuntime().availableProcessors()
        if (threads != null && !threads.isEmpty()) {
          i_threads = Integer.parseInt(threads)
        }

        var sb = ServerBuilder.forPort(port)
                              .addService(HelloWorldService())

        val value = System.getenv().getOrDefault("JVM_EXECUTOR_TYPE", "workStealing")
        when (value) {
          "direct" -> sb = sb.directExecutor()
          "single" -> sb = sb.executor(Executors.newSingleThreadExecutor())
          "fixed" -> sb = sb.executor(Executors.newFixedThreadPool(i_threads))
          "workStealing" -> sb = sb.executor(Executors.newWorkStealingPool(i_threads))
          "cached" -> sb = sb.executor(Executors.newCachedThreadPool())
        }

        server = sb.build()
    }

    fun start() {
        server.start()
        println("Server started, listening on $port")
        Runtime.getRuntime().addShutdownHook(
                Thread {
                    println("*** shutting down gRPC server since JVM is shutting down")
                    this@HelloWorldServer.stop()
                    println("*** server shut down")
                }
        )
    }

    private fun stop() {
        server.shutdown()
    }

    fun blockUntilShutdown() {
        server.awaitTermination()
    }

    private class HelloWorldService : GreeterGrpcKt.GreeterCoroutineImplBase() {
        override suspend fun sayHello(request: HelloRequest) = HelloReply
                .newBuilder()
                .setResponse(request.getRequest())
                .build()
    }
}

fun main() {
    val port = 50051
    val server = HelloWorldServer(port)
    server.start()
    server.blockUntilShutdown()
}
