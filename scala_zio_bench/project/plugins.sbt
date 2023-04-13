val zioGrpcVersion = "0.6.0-test5"

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")

libraryDependencies ++= Seq(
  "com.thesamet.scalapb.zio-grpc" %% "zio-grpc-codegen" % zioGrpcVersion,
  "com.thesamet.scalapb" %% "compilerplugin" % "0.11.13"
)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.1")
