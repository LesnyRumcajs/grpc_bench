FROM bellsoft/liberica-openjdk-centos:17.0.2

WORKDIR /app
COPY java_micronaut_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew installDist

ENV GC "-XX:+UseParallelGC"
ENV JAVA_OPTS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70 -XX:+AlwaysPreTouch -XX:-UsePerfData"

ENTRYPOINT ["/app/build/install/app/bin/app"]

