package io.grpc.examples.helloworld

import cats.effect.IO
import io.grpc.examples.helloworld.helloworld.GreeterGrpc.Greeter
import io.grpc.examples.helloworld.helloworld.{GreeterFs2Grpc, HelloReply, HelloRequest}

import scala.concurrent.Future


class GreeterServiceImpl extends GreeterFs2Grpc[IO, io.grpc.Metadata] {
  override def sayHello(request: HelloRequest, ctx: io.grpc.Metadata): IO[HelloReply] =
    IO.pure(HelloReply(request.request))
}
