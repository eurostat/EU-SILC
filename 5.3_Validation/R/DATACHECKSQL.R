#PARAMETERS
#SPECIFY THE YEAR, YOUR COUNTRY, the version of the csv files and the PATH where the project is stored
####################################################################################################
YYYY <- 2024
CC <- "NL"
VERSION <- 1
EUSILC <- "V:/"
####################################################################################################
#END OF PARAMETERS



#libraries needed
library(sqldf)
library(DBI)
library(dplyr)

#folder where R files are stored
setwd(gsub(" ", "",paste(EUSILC,"5.3_Validation/R/",YYYY)))
wd<-getwd()

#folder where csv files are stored
PATH <- paste(EUSILC,"main/",tolower(CC),"/csv/")

#import csv files
D<-read.csv(gsub(" ", "",paste(PATH,"/SILC_RD_A_",toupper(CC),"_",YYYY,"_0000_V000",VERSION,".CSV")), header=TRUE)
H<-read.csv(gsub(" ", "",paste(PATH,"/SILC_RH_A_",toupper(CC),"_",YYYY,"_0000_V000",VERSION,".CSV")), header=TRUE)
R<-read.csv(gsub(" ", "",paste(PATH,"/SILC_RR_A_",toupper(CC),"_",YYYY,"_0000_V000",VERSION,".CSV")), header=TRUE)
P<-read.csv(gsub(" ", "",paste(PATH,"/SILC_RP_A_",toupper(CC),"_",YYYY,"_0000_V000",VERSION,".CSV")), header=TRUE)


#create a SQLlite database and importing 4 files as tables
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "D", D)
dbWriteTable(con, "H", H)
dbWriteTable(con, "R", R)
dbWriteTable(con, "P", P)



#importing all the R files with SQl queries
source(gsub(" ", "",paste("sql_prevalid_sval_D_valvsflag_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_sval_H_valvsflag_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_sval_R_valvsflag_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_sval_P_valvsflag_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_sval_missing_NA2_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_sval_wrong_NA2_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_sval_missing_NA3_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_sval_wrong_NA3_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_sval_missing_NA4_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_sval_wrong_NA4_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_lval_",YYYY,".R")))
source(gsub(" ", "",paste("sql_prevalid_summary_",YYYY,".R")))

#putting results in R datasets

#summary
SUMMARY_SQL_PREVALID <- dbReadTable(con, gsub(" ", "",paste("SUMMARY_SQL_PREVALID_",YYYY)))

#values vs flags errors
if (nrow(dbReadTable(con, "SVAL_D_VALvsFL")) > 0) {SVAL_D_VALvsFL <- dbReadTable(con, "SVAL_D_VALvsFL")}
if (nrow(dbReadTable(con, "SVAL_H_VALvsFL")) > 0) {SVAL_H_VALvsFL <- dbReadTable(con, "SVAL_H_VALvsFL")}
if (nrow(dbReadTable(con, "SVAL_R_VALvsFL")) > 0) {SVAL_R_VALvsFL <- dbReadTable(con, "SVAL_R_VALvsFL")}
if (nrow(dbReadTable(con, "SVAL_P_VALvsFL")) > 0) {SVAL_P_VALvsFL <- dbReadTable(con, "SVAL_P_VALvsFL")}

#non selected respondents errors
if (nrow(dbReadTable(con, "SVAL_P_NA3_M")) > 0) {SVAL_P_NA3_M <- dbReadTable(con, "SVAL_P_NA3_M")}
if (nrow(dbReadTable(con, "SVAL_P_NA3_W")) > 0) {SVAL_P_NA3_W <- dbReadTable(con, "SVAL_P_NA3_W")}


# loop to generate routing conditions and logical checks datasets
#select distinct ID
ID_FOR_LOOP_NA2_M <- SUMMARY_SQL_PREVALID %>% filter(grepl("NA2_M$", trimws(ID))) %>%  distinct(ID) %>% select(ID)
ID_FOR_LOOP_NA2_W <- SUMMARY_SQL_PREVALID %>% filter(grepl("NA2_W$", trimws(ID))) %>%  distinct(ID) %>% select(ID)
ID_FOR_LOOP_LVAL  <- SUMMARY_SQL_PREVALID %>% filter(grepl("^LVAL" , trimws(ID))) %>%  distinct(ID) %>% select(ID)
#loops
if (nrow(ID_FOR_LOOP_NA2_M) > 0) {
  for (i in 1:nrow(ID_FOR_LOOP_NA2_M)) {
  valeur <- ID_FOR_LOOP_NA2_M$ID[i]
  assign(ID_FOR_LOOP_NA2_M$ID[i], dbReadTable(con, gsub(" ", "",ID_FOR_LOOP_NA2_M$ID[i])))
  }
}
if (nrow(ID_FOR_LOOP_NA2_W) > 0) {
  for (i in 1:nrow(ID_FOR_LOOP_NA2_W)) {
  valeur <- ID_FOR_LOOP_NA2_W$ID[i]
  assign(ID_FOR_LOOP_NA2_W$ID[i], dbReadTable(con, gsub(" ", "",ID_FOR_LOOP_NA2_W$ID[i])))
  }
}
if (nrow(ID_FOR_LOOP_LVAL) > 0) {
  for (i in 1:nrow(ID_FOR_LOOP_LVAL)) {
  valeur <- ID_FOR_LOOP_LVAL$ID[i]
  assign(ID_FOR_LOOP_LVAL$ID[i], dbReadTable(con, gsub(" ", "",ID_FOR_LOOP_LVAL$ID[i])))
  }
}
rm(valeur)
rm(i)
rm(ID_FOR_LOOP_NA2_M)
rm(ID_FOR_LOOP_NA2_W)
rm(ID_FOR_LOOP_LVAL)