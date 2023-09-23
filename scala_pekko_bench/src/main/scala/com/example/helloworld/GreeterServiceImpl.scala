package io.grpc.examples.helloworld

//#import
import scala.concurrent.Future

import org.apache.pekko.NotUsed
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.stream.scaladsl.BroadcastHub
import org.apache.pekko.stream.scaladsl.Keep
import org.apache.pekko.stream.scaladsl.MergeHub
import org.apache.pekko.stream.scaladsl.Sink
import org.apache.pekko.stream.scaladsl.Source

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
