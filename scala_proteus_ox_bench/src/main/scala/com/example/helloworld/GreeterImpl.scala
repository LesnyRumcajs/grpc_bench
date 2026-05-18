package com.example.helloworld

object GreeterImpl {
  def sayHello(request: HelloRequest): HelloReply =
    HelloReply(request.request)
}
