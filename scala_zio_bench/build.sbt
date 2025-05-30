name := "zio-grpc-quickstart-scala"

version := "1.0"

scalaVersion := "2.13.16"

run / fork := true

val grpcVersion = "1.64.2"

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
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
}
