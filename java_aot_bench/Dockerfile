FROM ghcr.io/graalvm/graalvm-ce:ol8-java11-21.1.0 as rel

WORKDIR /app
COPY java_aot_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew assemble

RUN gu install native-image

RUN native-image --no-server --static --no-fallback -jar /app/build/libs/app-0.1-all.jar

ENTRYPOINT [ "/app/hello-world-java" ]
