package io.grpc.examples.helloworld

//#import


import java.io.File
import java.nio.file.Files
import java.security.KeyStore
import java.security.SecureRandom
import java.security.cert.Certificate
import java.security.cert.CertificateFactory

import scala.io.Source

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.http.scaladsl.ConnectionContext
import akka.http.scaladsl.Http
import akka.http.scaladsl.HttpConnectionContext
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.pki.pem.DERPrivateKeyLoader
import akka.pki.pem.PEMDecoder
import akka.stream.SystemMaterializer
import com.typesafe.config.ConfigFactory
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
//#import


//#server
object GreeterServer {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem[Nothing](Behaviors.empty, "GreeterServer")
    new GreeterServer()(system).run()
  }
}

class GreeterServer(implicit system: ActorSystem[_]) {

  def run(): Future[Http.ServerBinding] = {
    implicit val ec: ExecutionContext = system.executionContext

    val service: HttpRequest => Future[HttpResponse] =
      GreeterHandler(new GreeterServiceImpl(system))

    println(s"Parallel: ${system.settings.config.getString("akka.actor.default-dispatcher.fork-join-executor.parallelism-max")} GRPC_SERVER_CPUS: ${sys.env.get("GRPC_SERVER_CPUS")}")

    // Akka HTTP 10.1 requires adapters to accept the new actors APIs
    val bound = Http()(system.toClassic).bindAndHandleAsync(
      service,
      interface = "0.0.0.0",
      port = 50051,
      connectionContext = HttpConnectionContext()
    )(SystemMaterializer(system).materializer)

    bound.foreach { binding =>
      println(s"gRPC server bound to: ${binding.localAddress}")
    }

    bound
  }
  //#server

}
//#server
