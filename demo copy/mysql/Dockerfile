FROM mysql:latest

RUN apt-get update && apt-get install -y \
    curl \
    unzip \
&& rm -rf /var/lib/apt/lists/*

RUN \
mkdir /opt/employees_db && \
curl -L https://github.com/datacharmer/test_db/archive/master.zip > /opt/employees_db/download.zip && \
unzip /opt/employees_db/download.zip -d /opt/employees_db && rm /opt/employees_db/download.zip

COPY create_database.sh /docker-entrypoint-initdb.d/create_database.sh

RUN chmod +x /docker-entrypoint-initdb.d/create_database.sh

ENV MYSQL_DATABASE=employees
ENV MYSQL_ROOT_PASSWORD=mysql
ENV MYSQL_USER=presto
ENV MYSQL_PASSWORD=mysql