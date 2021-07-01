name := "akka-grpc-quickstart-scala"

version := "1.0"

scalaVersion := "2.13.6"

run / fork := true

val akkaVersion = "2.6.15+8-dd289e7e-SNAPSHOT"
val akkaHttpVersion = "10.2.5-M1"

enablePlugins(AkkaGrpcPlugin)

// to get latest versions
resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-parsing" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-discovery" % akkaVersion,
  "com.typesafe.akka" %% "akka-pki" % akkaVersion,
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.1.1" % Test
)

// Akka and Google provided proto files seem to differ a bit so we need to choose
// (doesn't seem to be important)
assembly / assemblyMergeStrategy := {
  case PathList(ps @ _*) if ps.last endsWith ".proto" => MergeStrategy.first
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
