FROM openjdk:11.0.8-slim-buster

WORKDIR /app
COPY java_micronaut /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew installDist

ENV JAVA_OPTS "-XX:+MinRAMPercentage=70 -XX:+MaxRAMPercentage=70"

ENTRYPOINT /app/build/install/app/bin/app
