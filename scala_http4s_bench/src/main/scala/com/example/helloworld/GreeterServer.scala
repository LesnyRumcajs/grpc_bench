package io.grpc.examples.helloworld

import cats.effect._
import io.grpc.examples.helloworld.helloworld._

import java.util.concurrent.Executors
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s._
import cats.Applicative
import org.http4s._


object GreeterServer extends IOApp {

  def serviceRoutes[F[_]: Temporal]: HttpApp[F] = Greeter.toRoutes(new GreeterImpl).orNotFound

  def server[F[_]: Async] = EmberServerBuilder
    .default[F]
    .withHttp2
    .withHost(ipv4"0.0.0.0")
    .withPort(port"50051")
    .withHttpApp(serviceRoutes)
    .build
    
  def run(args: List[String]): IO[ExitCode] = 
    server[IO].useForever
}

