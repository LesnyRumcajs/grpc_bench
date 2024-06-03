FROM eclipse-temurin:22-jdk-jammy

WORKDIR /app
COPY java_vertx_grpc_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew assemble

ENV GC "-XX:+UseSerialGC"
ENV JAVA_OPTS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

ENTRYPOINT ["java", "-jar", "/app/build/libs/java_vertx_grpc_bench-all.jar" ]
