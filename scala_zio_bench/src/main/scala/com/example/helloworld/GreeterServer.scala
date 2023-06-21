package com.example.helloworld

import scalapb.zio_grpc.{ServerMain, ServiceList}

object GreeterServer extends ServerMain {
  def services: ServiceList[Any] = ServiceList.add(GreeterImpl)

  // Default port is 9000
  override def port = 50051
}
