FROM openjdk:18.0.1

WORKDIR /app
COPY java_hotspot_grpc_sgc_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew installDist

ENV GC "-XX:+UseSerialGC"
ENV JAVA_OPTS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

ENTRYPOINT [ "/app/build/install/examples/bin/hello-world-server" ]
