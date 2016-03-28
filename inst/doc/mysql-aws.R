## ----setup, echo=F, message=F--------------------------------------------
library(RMySQL)
library(dplyr)
library(krsp)

## ----rmysql--------------------------------------------------------------
db <-  dbConnect(MySQL(), group = "krsp-aws")
dbListTables(db)

## ----src-mysql-----------------------------------------------------------
db <- src_mysql(group = "krsp-aws", dbname = NULL, password = NULL, user = NULL)
src_tbls(db)

## ----krsp-connect--------------------------------------------------------
db <- krsp_connect(group = "krsp-aws")
krsp_tables(db)

