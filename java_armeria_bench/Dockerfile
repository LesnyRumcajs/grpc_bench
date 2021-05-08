FROM eclipse-temurin:17

WORKDIR /app
COPY java_armeria_bench /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew installDist

ENV GC "-XX:+UseG1GC"
ENV JAVA_OPTS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70 -Dcom.linecorp.armeria.validateHeaders=false"

ENTRYPOINT [ "/app/build/install/app/bin/hello-world-server" ]
