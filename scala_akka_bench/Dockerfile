FROM hseeberger/scala-sbt:11.0.7_1.3.13_2.11.12 as BUILDER

WORKDIR /app

# Make sure docker can cache a few static setup things

# initialize only sbt
RUN mkdir -p /app/project
COPY scala_akka_bench/project/build.properties /app/project
RUN sbt exit

# initialize plugins
COPY scala_akka_bench/project /app/project
RUN sbt exit

# initialize full build
COPY scala_akka_bench/*.sbt /app
# to initialize compiler
RUN mkdir -p /app/src/main/scala/
RUN touch /app/src/main/scala/Dummy.scala
RUN sbt compile

# Actually build project
COPY scala_akka_bench/src /app/src
COPY proto/helloworld/helloworld.proto /app/src/main/protobuf/helloworld.proto

RUN sbt assembly

FROM openjdk:18.0.1

ENV GC "-XX:+UseParallelGC"
ENV _JAVA_OPTIONS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

COPY --from=builder /app/target/scala-2.13/akka-grpc-quickstart-scala-assembly-1.0.jar .

ENTRYPOINT ["java", "-jar", "akka-grpc-quickstart-scala-assembly-1.0.jar"]
