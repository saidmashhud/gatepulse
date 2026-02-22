FROM eclipse-temurin:21-jre-alpine

WORKDIR /app

COPY target/scala-3.3.3/hookline-bridge-assembly-*.jar app.jar

ENTRYPOINT ["java", "-XX:+UseZGC", "-XX:MaxRAMPercentage=75", "-jar", "app.jar"]
