FROM adoptopenjdk:14.0.2_8-jre-openj9-0.21.0-bionic

WORKDIR /app
COPY java_hotspot_grpc_sgc_bench /app
COPY java_openj9_grpc_gencon_bench/populate_scc.sh /app
COPY proto/helloworld/helloworld.proto /app/src/main/proto/helloworld.proto

RUN /app/gradlew installDist

ENV GC "-Xpolicy:optthruput"
ENV JAVA_OPTS "${GC} -XX:MinRAMPercentage=70 -XX:MaxRAMPercentage=70"

RUN /app/populate_scc.sh
ENV JAVA_OPTS "${JAVA_OPTS} -Xshareclasses:name=grcp,cacheDir=/app/.classCache"

ENTRYPOINT [ "/app/build/install/examples/bin/hello-world-server" ]
