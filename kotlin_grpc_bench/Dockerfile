FROM adoptopenjdk:14.0.2_8-jre-hotspot-bionic 

WORKDIR /app
COPY kotlin_grpc_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/hello_world.proto

RUN /app/gradlew installDist

ENV PGC "-XX:+UseParallelGC -XX:ParallelGCThreads=1"
ENV JAVA_OPTS "${PGC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

ENTRYPOINT [ "/app/build/install/examples/bin/hello-world-server" ]


