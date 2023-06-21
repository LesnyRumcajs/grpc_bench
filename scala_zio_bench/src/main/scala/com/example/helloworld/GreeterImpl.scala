package com.example.helloworld

import io.grpc.StatusException
import io.grpc.examples.helloworld.helloworld.ZioHelloworld.Greeter
import io.grpc.examples.helloworld.helloworld.{HelloReply, HelloRequest}
import zio.{IO, ZIO}


object GreeterImpl extends Greeter {
  override def sayHello(request: HelloRequest): IO[StatusException, HelloReply] =
    ZIO.succeed(HelloReply(request.request))
}
