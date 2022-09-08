package com.example.helloworld

import io.grpc.Status
import io.grpc.examples.helloworld.helloworld.ZioHelloworld.ZGreeter
import io.grpc.examples.helloworld.helloworld.{HelloReply, HelloRequest}
import zio.ZIO


object GreeterImpl extends ZGreeter[Any, Any] {
  override def sayHello(request: HelloRequest): ZIO[Any, Status, HelloReply] =
    ZIO.succeed(HelloReply(request.request))
}
