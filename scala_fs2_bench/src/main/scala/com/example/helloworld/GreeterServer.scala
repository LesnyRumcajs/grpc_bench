package io.grpc.examples.helloworld

import cats.effect._
import fs2.grpc.syntax.all._
import io.grpc.examples.helloworld.helloworld.GreeterFs2Grpc
import io.grpc.{ServerBuilder, ServerServiceDefinition}

import java.util.concurrent.Executors


object GreeterServer extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val service: Resource[IO, ServerServiceDefinition] =
      GreeterFs2Grpc.bindServiceResource[IO](new GreeterServiceImpl())

    def serverBuilder: ServerBuilder[_] = {
      val sb = ServerBuilder.forPort(50051)

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
      sb
    }

    def run(service: ServerServiceDefinition) = serverBuilder
      .addService(service)
      .resource[IO]
      .evalMap(server => IO(server.start()))
      .useForever

    service.use(run)
  }
}

