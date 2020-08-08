FROM openjdk:8-jdk

WORKDIR /app
COPY kotlin_grpc_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/hello_world.proto

RUN /app/gradlew installDist

ENTRYPOINT [ "/app/build/install/examples/bin/hello-world-server" ]


