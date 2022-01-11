package io.grpc.examples.helloworld

//#import
import scala.concurrent.Future

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.BroadcastHub
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.MergeHub
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source

//#import

//#service-request-reply
//#service-stream
class GreeterServiceImpl(system: ActorSystem[_]) extends Greeter {
  private implicit val sys: ActorSystem[_] = system

  //#service-request-reply
  val (inboundHub: Sink[HelloRequest, NotUsed], outboundHub: Source[HelloReply, NotUsed]) =
    MergeHub.source[HelloRequest]
    .map(request => HelloReply(request.request))
      .toMat(BroadcastHub.sink[HelloReply])(Keep.both)
      .run()
  //#service-request-reply

  override def sayHello(request: HelloRequest): Future[HelloReply] = {
    Future.successful(HelloReply(request.request))
  }
}
//#service-stream
//#service-request-reply
