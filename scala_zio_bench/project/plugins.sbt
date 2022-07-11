val zioGrpcVersion = "0.6.0-test1"

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")

libraryDependencies ++= Seq(
  "com.thesamet.scalapb.zio-grpc" %% "zio-grpc-codegen" % zioGrpcVersion,
  "com.thesamet.scalapb" %% "compilerplugin" % "0.11.11"
)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.2.0")
