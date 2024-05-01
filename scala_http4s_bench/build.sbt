name := "http4s-grpc-quickstart-scala"

version := "1.0"

scalaVersion := "3.4.1"
val http4sVersion = "0.23.26"

run / fork := true

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
)

enablePlugins(Http4sGrpcPlugin)
Compile / PB.targets ++= Seq(
  // set grpc = false because http4s-grpc generates its own code
  scalapb.gen(grpc = false) -> (Compile / sourceManaged).value / "scalapb"
)

