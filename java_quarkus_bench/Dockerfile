FROM eclipse-temurin:19.0.1_10-jdk-focal

WORKDIR /app

COPY java_quarkus_bench /app
COPY proto /app/src/main/proto

RUN /app/mvnw clean package

# Configure the JAVA_OPTIONS, you can add -XshowSettings:vm to also display the heap size.
ENV JAVA_OPTIONS="-Dquarkus.http.host=0.0.0.0 -Djava.util.logging.manager=org.jboss.logmanager.LogManager"

ENTRYPOINT java -Dquarkus.grpc.server.instances="${GRPC_SERVER_CPUS:-1}" -jar /app/target/quarkus-app/quarkus-run.jar
