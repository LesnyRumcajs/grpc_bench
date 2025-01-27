FROM sbtscala/scala-sbt:eclipse-temurin-23.0.1_11_1.10.7_3.6.3 as BUILDER

WORKDIR /app
COPY scala_pekko_bench/ /app
COPY proto/helloworld/helloworld.proto /app/src/main/protobuf/helloworld.proto

RUN sbt assembly

FROM eclipse-temurin:23-jdk-noble

ENV GC "-XX:+UseParallelGC"
ENV _JAVA_OPTIONS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

COPY --from=builder /app/target/scala-2.13/pekko-grpc-quickstart-scala-assembly-1.0.jar .

ENTRYPOINT ["java", "-jar", "pekko-grpc-quickstart-scala-assembly-1.0.jar"]
