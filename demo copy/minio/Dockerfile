FROM minio/minio:latest

# default admin creds for minio container minioadmin:minioadmin

EXPOSE 9000
RUN apt-get update && apt-get install -y procps
VOLUME ["/data"]

CMD [minio]