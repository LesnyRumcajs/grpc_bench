FROM hseeberger/scala-sbt:11.0.7_1.3.13_2.11.12

WORKDIR /app
COPY scala_akka /app
COPY proto/helloworld/helloworld.proto /app/src/main/protobuf/helloworld.proto

RUN sbt compile
RUN sbt assembly

ENV JAVA_OPTS "-XX:+UseParallelGC -XX:ParallelGCThreads=2 -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

ENTRYPOINT ["/usr/local/openjdk-11/bin/java", "$JAVA_OPTS", "-jar", "/app/target/scala-2.13/akka-grpc-quickstart-scala-assembly-1.0.jar"]
