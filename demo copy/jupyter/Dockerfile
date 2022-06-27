FROM jupyter/scipy-notebook:latest
USER root
RUN apt-get -o Acquire::Check-Valid-Until=false -o Acquire::Check-Date=false update -y && apt-get install -qy graphviz libmysqlclient-dev mongodb-clients mysql-client wget host
RUN pip install --no-cache-dir --trusted-host=pypi.python.org --trusted-host=pypi.org --trusted-host=files.pythonhosted.org --upgrade trino pyhive graphviz mysqlclient mongo
ADD notebooks/ /home/jovyan/