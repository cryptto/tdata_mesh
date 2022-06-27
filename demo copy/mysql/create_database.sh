#!/usr/bin/env bash
#cd /opt/employees_db/test_db-master
mysql -u root -p "mysql" -e "CREATE database hive_metastore; use hive_metastore; source
/opt/hive/scripts/metastore/upgrade/mysql/hive-schema-3.0.0.mysql.sql;"