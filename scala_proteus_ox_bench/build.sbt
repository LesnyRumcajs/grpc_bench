name := "proteus-ox-grpc-quickstart-scala"

version := "1.0"

scalaVersion := "3.3.7"

run / fork := true

val grpcVersion    = "1.81.0"
val proteusVersion = "0.4.0"

libraryDependencies ++= Seq(
  "com.github.ghostdogpr" %% "proteus-grpc-ox" % proteusVersion,
  "io.grpc"                % "grpc-netty"      % grpcVersion
)

scalacOptions ++= Seq("-no-indent", "-Ykind-projector")

assembly / assemblyMergeStrategy := {
  case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
  case x if x.endsWith("module-info.class")            => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
