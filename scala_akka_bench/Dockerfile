FROM hseeberger/scala-sbt:11.0.7_1.3.13_2.11.12

WORKDIR /app
COPY scala_akka_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/protobuf/helloworld.proto

RUN sbt compile
RUN sbt assembly

ENV GC "-XX:+UseParallelGC"
ENV JAVA_OPTS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

ENTRYPOINT ["/usr/local/openjdk-11/bin/java", "-jar", "/app/target/scala-2.13/akka-grpc-quickstart-scala-assembly-1.0.jar"]
