val zioGrpcVersion = "0.6.3"

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.7")

libraryDependencies ++= Seq(
  "com.thesamet.scalapb.zio-grpc" %% "zio-grpc-codegen" % zioGrpcVersion,
  "com.thesamet.scalapb" %% "compilerplugin" % "0.11.17"
)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")
