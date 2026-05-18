package com.example.helloworld

import java.util.concurrent.Executors

import io.grpc.netty.NettyServerBuilder
import ox.{inScopeRunner, never, supervised}

import proteus.server.{OxServerBackend, ServerService}

object GreeterServer {
  def main(args: Array[String]): Unit = supervised {
    val backend = OxServerBackend(inScopeRunner())
    val service = ServerService(using backend)
      .rpc(sayHelloRpc, GreeterImpl.sayHello)
      .build(greeterService)

    val sb = NettyServerBuilder.forPort(50051).addService(service)

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
    val threads = System.getenv("GRPC_SERVER_CPUS")
    var iThreads = Runtime.getRuntime.availableProcessors
    if (threads != null && threads.nonEmpty) iThreads = threads.toInt
    val value = Option(System.getenv("JVM_EXECUTOR_TYPE")).getOrElse("workStealing")
    value match {
      case "direct"       => sb.directExecutor(): Unit
      case "single"       => sb.executor(Executors.newSingleThreadExecutor()): Unit
      case "fixed"        => sb.executor(Executors.newFixedThreadPool(iThreads)): Unit
      case "workStealing" => sb.executor(Executors.newWorkStealingPool(iThreads)): Unit
      case "cached"       => sb.executor(Executors.newCachedThreadPool()): Unit
    }

    val server = sb.build().start()
    sys.addShutdownHook {
      server.shutdown()
      ()
    }
    never
  }
}
