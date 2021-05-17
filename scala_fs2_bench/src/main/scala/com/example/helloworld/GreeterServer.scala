package io.grpc.examples.helloworld

import cats.effect._
import fs2.grpc.syntax.all._
import io.grpc.examples.helloworld.helloworld.GreeterFs2Grpc
import io.grpc.{ServerBuilder, ServerServiceDefinition}


object GreeterServer extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val service: Resource[IO, ServerServiceDefinition] =
      GreeterFs2Grpc.bindServiceResource[IO](new GreeterServiceImpl())

    def run(service: ServerServiceDefinition) = ServerBuilder
      .forPort(50051)
      .addService(service)
      .resource[IO]
      .evalMap(server => IO(server.start()))
      .useForever

    service.use(run)
  }
}

