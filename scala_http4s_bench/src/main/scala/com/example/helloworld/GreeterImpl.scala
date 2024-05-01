package io.grpc.examples.helloworld

import cats.Applicative
import cats.syntax.all._
import org.http4s.Headers
import io.grpc.examples.helloworld.helloworld._

class GreeterImpl[F[_]: Applicative] extends Greeter[F] {
  override def sayHello(request: HelloRequest, ctx: Headers): F[HelloReply] = 
    HelloReply(request.request).pure
}
