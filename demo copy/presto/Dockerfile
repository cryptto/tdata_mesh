FROM openjdk:11-jdk
EXPOSE 8080

ARG TRINO_VERSION=356
ENV TRINO_PKG trino-server-$TRINO_VERSION.tar.gz
ENV TRINO_PKG_URL https://repo1.maven.org/maven2/io/trino/trino-server/$TRINO_VERSION/$TRINO_PKG

ENV TRINO_CLI_JAR_URL https://repo1.maven.org/maven2/io/trino/trino-cli/$TRINO_VERSION/trino-cli-$TRINO_VERSION-executable.jar

# Install python to run the launcher script
# RUN apt-get update && apt-get install -y python less && apt-get install dialog apt-utils -y
RUN apt-get -o Acquire::Check-Valid-Until=false -o Acquire::Check-Date=false update && apt-get install -y python less && apt-get install dialog apt-utils -y

# Download Trino package
# Use curl rather ADD <remote> to leverage RUN caching
RUN curl -o $TRINO_PKG $TRINO_PKG_URL && tar -zxf /$TRINO_PKG

# Create directory for Trino data
RUN mkdir -p /var/lib/presto/data

# Add Trino configuration
WORKDIR /trino-server-$TRINO_VERSION
RUN mkdir etc
ADD etc/jvm.config etc/
ADD etc/config.properties etc/
ADD etc/node.properties etc/
ADD etc/catalog etc/catalog
RUN rm etc/catalog/swapi.properties
# Add Trino SWAPI plugin
#RUN mkdir plugin/swapi

# Download Trino CLI
RUN mkdir -p bin
RUN curl -o bin/trino-cli $TRINO_CLI_JAR_URL
RUN chmod +x bin/trino-cli

CMD bin/launcher.py run
