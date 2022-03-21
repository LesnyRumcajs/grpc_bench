val zioGrpcVersion = "0.4.2"

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")

libraryDependencies += "com.thesamet.scalapb.zio-grpc" %% "zio-grpc-codegen" % zioGrpcVersion

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")
