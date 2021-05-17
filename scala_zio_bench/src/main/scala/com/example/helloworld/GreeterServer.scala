package io.grpc.examples.helloworld

import io.grpc.ServerBuilder
import scalapb.zio_grpc.{ServerMain, ServiceList}
import zio.ZEnv

import java.util.concurrent.Executors

object MyMain extends ServerMain {
  def services = ServiceList.add(GreeterImpl)

  override def builder = {
    val sb = super.builder

    /**
     * Allow customization of the Executor with two environment variables:
     *
     * <p>
     * <ul>
     * <li>JVM_EXECUTOR_TYPE: direct, workStealing, single, fixed, cached</li>
     * <li>JVM_EXECUTOR_THREADS: integer value.</li>
     * </ul>
     * </p>
     *
     * The number of Executor Threads will default to the number of
     * availableProcessors(). Only the workStealing and fixed executors will use
     * this value.
     */
    val threads = System.getenv("JVM_EXECUTOR_THREADS")
    var i_threads = Runtime.getRuntime.availableProcessors
    if (threads != null && !threads.isEmpty) i_threads = threads.toInt
    /*
         * Use a Direct Executor by default (best performance) since the GRPC
         * service in this code is guaranteed non-blocking
         */
    val value = System.getenv.getOrDefault("JVM_EXECUTOR_TYPE", "workStealing")
    value match {
      case "direct" => sb.directExecutor
      case "single" => sb.executor(Executors.newSingleThreadExecutor)
      case "fixed" => sb.executor(Executors.newFixedThreadPool(i_threads))
      case "workStealing" => sb.executor(Executors.newWorkStealingPool(i_threads))
      case "cached" => sb.executor(Executors.newCachedThreadPool)
    }
    sb
  }

  // Default port is 9000
  override def port = 50051
}
