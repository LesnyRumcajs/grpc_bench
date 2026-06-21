name := "pekko-grpc-quickstart-scala"

version := "1.0"

scalaVersion := "2.13.18"

run / fork := true

val pekkoVersion = "2.0.0-M3"
val pekkoHttpVersion = "2.0.0-M1"
val pekkoGrpcVersion = "2.0.0-M2"

enablePlugins(PekkoGrpcPlugin)

// to get latest versions
resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += Resolver.defaultLocal

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.5.16",
  "org.apache.pekko" %% "pekko-actor-typed" % pekkoVersion,
  "org.apache.pekko" %% "pekko-http" % pekkoHttpVersion,
  "org.apache.pekko" %% "pekko-http-core" % pekkoHttpVersion,
  "org.apache.pekko" %% "pekko-parsing" % pekkoHttpVersion,
  "org.apache.pekko" %% "pekko-discovery" % pekkoVersion,
  "org.apache.pekko" %% "pekko-pki" % pekkoVersion,
  "org.apache.pekko" %% "pekko-slf4j" % pekkoVersion,
  "org.apache.pekko" %% "pekko-grpc-runtime" % pekkoGrpcVersion,
  "org.apache.pekko" %% "pekko-actor-testkit-typed" % pekkoVersion % Test,
  "org.apache.pekko" %% "pekko-stream-testkit" % pekkoVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

dependencyOverrides ++= Seq(
  "org.apache.pekko" %% "pekko-http" % pekkoHttpVersion,
  "org.apache.pekko" %% "pekko-http-core" % pekkoHttpVersion,
  "org.apache.pekko" %% "pekko-parsing" % pekkoHttpVersion
)

evictionErrorLevel := Level.Warn

libraryDependencySchemes += "org.apache.pekko" %% "pekko-actor-testkit-typed" % VersionScheme.Always
libraryDependencySchemes += "org.apache.pekko" %% "pekko-stream-testkit" % VersionScheme.Always
// (doesn't seem to be important)
assembly / mainClass := Some("io.grpc.examples.helloworld.GreeterServer")

assembly / assemblyMergeStrategy := {
  case PathList(ps @ _*) if ps.last endsWith ".proto" => MergeStrategy.first
  case PathList("module-info.class") => MergeStrategy.last
  case path if path.endsWith("/module-info.class") => MergeStrategy.last
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
