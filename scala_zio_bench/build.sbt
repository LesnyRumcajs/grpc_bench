name := "zio-grpc-quickstart-scala"

version := "1.0"

scalaVersion := "2.13.11"

run / fork := true

val grpcVersion = "1.52.1"

Compile / PB.targets := Seq(
    scalapb.gen(grpc = true) -> (Compile / sourceManaged).value,
    scalapb.zio_grpc.ZioCodeGenerator -> (Compile / sourceManaged).value,
)

libraryDependencies ++= Seq(
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
    "io.grpc" % "grpc-netty" % grpcVersion
)

// https://scalapb.github.io/docs/grpc/#grpc-netty-issues
assembly / assemblyMergeStrategy := {
    case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
    case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
}
