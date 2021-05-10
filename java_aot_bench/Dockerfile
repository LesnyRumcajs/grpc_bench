FROM ghcr.io/graalvm/graalvm-ce:ol8-java11-19.3.5 as rel

WORKDIR /app
COPY java_micronaut_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew assemble

RUN gu install native-image

RUN native-image --no-server --static --no-fallback -jar /app/build/libs/app-0.1-all.jar

ENTRYPOINT [ "/app/app-0.1-all" ]
