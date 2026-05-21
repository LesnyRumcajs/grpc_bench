package io.grpc.examples.helloworld

//#import
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{HttpRequest, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}
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
      GreeterHandler(new GreeterServiceImpl)

    println(s"Parallel: ${system.settings.config.getString("pekko.actor.default-dispatcher.fork-join-executor.parallelism-max")} GRPC_SERVER_CPUS: ${sys.env.get("GRPC_SERVER_CPUS")}")

    // Pekko HTTP 10.1 requires adapters to accept the new actors APIs
    val bound = Http()(system.toClassic).newServerAt(
      interface = "0.0.0.0",
      port = 50051
    ).bind(service)

    bound.foreach { binding =>
      println(s"gRPC server bound to: ${binding.localAddress}")
    }

    bound
  }
  //#server

}
//#server
