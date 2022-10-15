name := "fs2-grpc-quickstart-scala"

version := "1.0"

scalaVersion := "2.13.10"

run / fork := true

enablePlugins(Fs2Grpc)

libraryDependencies ++= Seq(
  "io.grpc" % "grpc-netty-shaded" % scalapb.compiler.Version.grpcJavaVersion
)
