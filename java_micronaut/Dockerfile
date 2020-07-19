FROM openjdk:8-jdk

WORKDIR /app
COPY java_micronaut /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew installDist

ENTRYPOINT /app/build/install/app/bin/app

