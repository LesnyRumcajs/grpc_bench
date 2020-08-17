FROM adoptopenjdk:14.0.2_8-jre-hotspot-bionic

WORKDIR /app
COPY java_micronaut_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew installDist

ENV JAVA_OPTS "-XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

ENTRYPOINT ["/app/build/install/app/bin/app"]

