FROM openjdk:17

WORKDIR /app
COPY kotlin_grpc_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/hello_world.proto

RUN /app/gradlew installDist

ENV GC "-XX:+UseParallelGC"
ENV JAVA_OPTS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

ENTRYPOINT [ "/app/build/install/examples/bin/hello-world-server" ]
