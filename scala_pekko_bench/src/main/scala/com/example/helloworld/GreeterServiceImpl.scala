package io.grpc.examples.helloworld

//#import
import scala.concurrent.Future

//#import

//#service-request-reply
//#service-stream
class GreeterServiceImpl extends Greeter {
  override def sayHello(request: HelloRequest): Future[HelloReply] = {
    Future.successful(HelloReply(request.request))
  }
}
//#service-stream
//#service-request-reply
