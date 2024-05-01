FROM sbtscala/scala-sbt:eclipse-temurin-jammy-22_36_1.9.9_3.4.1 as BUILDER

WORKDIR /app
COPY scala_http4s_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/protobuf/helloworld.proto

RUN sbt assembly

FROM eclipse-temurin:22-jdk-jammy

ENV GC "-XX:+UseZGC -XX:+ZGenerational"
ENV _JAVA_OPTIONS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

COPY --from=builder /app/target/scala-3.4.1/http4s-grpc-quickstart-scala-assembly-1.0.jar .

ENTRYPOINT ["java", "-jar", "http4s-grpc-quickstart-scala-assembly-1.0.jar"]
