package com.example.helloworld

import scalapb.zio_grpc.{Server, ServerLayer, ServerMain, ServiceList}
import zio.ZLayer

import java.util.concurrent.Executors

object GreeterServer extends ServerMain {
  def services: ServiceList[Any] = ServiceList.add(GreeterImpl)

  // Default port is 9000
  override def port = 50051

  override def serverLive: ZLayer[Any, Throwable, Server] = {
    val sb = builder

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
    var i_threads = Runtime.getRuntime.availableProcessors
    if (threads != null && !threads.isEmpty) i_threads = threads.toInt
    val value = System.getenv.getOrDefault("JVM_EXECUTOR_TYPE", "workStealing")
    value match {
      case "direct" => sb.directExecutor
      case "single" => sb.executor(Executors.newSingleThreadExecutor)
      case "fixed" => sb.executor(Executors.newFixedThreadPool(i_threads))
      case "workStealing" => sb.executor(Executors.newWorkStealingPool(i_threads))
      case "cached" => sb.executor(Executors.newCachedThreadPool)
    }

    ServerLayer.fromServiceList(sb, services)
  }
}
