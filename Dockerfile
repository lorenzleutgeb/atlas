FROM openjdk:11
COPY lib/libz3java.so /lib/
COPY lac.jsh /
COPY lac.properties /
RUN echo "xyz.leutgeb.lorenz.lac.module.Loader.defaultHome=/examples" >> /lac.properties
COPY src/test/resources/examples /examples
COPY src/test/resources/tactics /tactics
COPY build/libs/lac-*-shadow.jar /lac.jar
ENTRYPOINT ["java", "-jar", "/lac.jar"]
