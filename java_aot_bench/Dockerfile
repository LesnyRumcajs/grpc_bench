FROM oracle/graalvm-ce:20.1.0-java11-ol8 as rel

WORKDIR /app
COPY java_micronaut /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew assemble

RUN gu install native-image

RUN native-image --no-server --static --no-fallback -jar /app/build/libs/app-0.1-all.jar

ENTRYPOINT [ "/app/app-0.1-all" ]
