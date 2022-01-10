package io.grpc.examples.helloworld

import io.grpc.Status
import io.grpc.examples.helloworld.helloworld.ZioHelloworld.ZGreeter
import io.grpc.examples.helloworld.helloworld.{HelloReply, HelloRequest}
import zio.{ZEnv, ZIO}


object GreeterImpl extends ZGreeter[ZEnv, Any] {
  override def sayHello(request: HelloRequest): ZIO[ZEnv, Status, HelloReply] =
    ZIO.succeed(HelloReply(request.request))
}
