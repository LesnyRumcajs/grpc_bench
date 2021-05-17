package io.grpc.examples.helloworld

import scalapb.zio_grpc.{ServerMain, ServiceList}

object MyMain extends ServerMain {
  def services = ServiceList.add(GreeterImpl)

  // Default port is 9000
  override def port: Int = 50051
}
