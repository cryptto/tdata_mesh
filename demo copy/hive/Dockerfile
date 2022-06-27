FROM openjdk:8-jre

ENV HIVE_VERSION=3.0.0
ENV HADOOP_VERSION=3.2.1
ENV HADOOP_HOME /opt/hadoop-$HADOOP_VERSION
ENV HADOOP_CONF_DIR /opt/hadoop-$HADOOP_VERSION/share/hadoop/conf:/opt/hive/conf
# Install Hive Metastore
RUN wget --no-check-certificate http://apache.claz.org/hive/hive-standalone-metastore-$HIVE_VERSION/hive-standalone-metastore-$HIVE_VERSION-bin.tar.gz
RUN tar xf hive-standalone-metastore-$HIVE_VERSION-bin.tar.gz
RUN mv apache-hive-metastore-$HIVE_VERSION-bin /opt
RUN ln -s /opt/apache-hive-metastore-$HIVE_VERSION-bin /opt/hive
RUN rm hive-standalone-metastore-$HIVE_VERSION-bin.tar.gz
ADD hive-site.xml /opt/hive/conf

# Install hadoop
RUN wget --no-check-certificate http://apache.mirrors.hoobly.com/hadoop/common/hadoop-$HADOOP_VERSION/hadoop-$HADOOP_VERSION.tar.gz
RUN tar xf hadoop-$HADOOP_VERSION.tar.gz
RUN rm hadoop-$HADOOP_VERSION.tar.gz && mv hadoop-$HADOOP_VERSION /opt/hadoop-$HADOOP_VERSION

RUN rm /opt/hive/lib/guava-19.0.jar && cp /opt/hadoop-$HADOOP_VERSION/share/hadoop/common/lib/guava-27.0-jre.jar /opt/hive/lib/
RUN mkdir -p /shared_data

# Install MySQL client jar for Hive
RUN wget --no-check-certificate https://dev.mysql.com/get/Downloads/Connector-J/mysql-connector-java-8.0.23.tar.gz
RUN tar xf mysql-connector-java-8.0.23.tar.gz
RUN mv mysql-connector-java-8.0.23/mysql-connector-java-8.0.23.jar /opt/hive/lib
RUN rm -rf mysql-connector-java-8.0.23
RUN rm -rf mysql-connector-java-8.0.23.tar.gz

CMD ["/opt/hive/bin/start-metastore"]
