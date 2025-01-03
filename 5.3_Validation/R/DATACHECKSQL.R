#PARAMETERS
#SPECIFY THE YEAR, YOUR COUNTRY, the version of the csv files and the PATH where the project is stored
####################################################################################################
YYYY <- 2024
CC <- "SE"
VERSION <- 1
EUSILC <- "V:/"
####################################################################################################
#END OF PARAMETERS



#libraries needed
library(sqldf)
library(DBI)

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
#values vs flags errors
SVAL_D_VALvsFL <- dbReadTable(con, "SVAL_D_VALvsFL")
SVAL_H_VALvsFL <- dbReadTable(con, "SVAL_H_VALvsFL")
SVAL_R_VALvsFL <- dbReadTable(con, "SVAL_R_VALvsFL")
SVAL_P_VALvsFL <- dbReadTable(con, "SVAL_P_VALvsFL")

#routing condition errors, examples
SVAL_PC310_NA2_M<- dbReadTable(con, "SVAL_PC310_NA2_M")
SVAL_PC310_NA2_W<- dbReadTable(con, "SVAL_PC310_NA2_W")

#non selected respondents errors
SVAL_P_NA3_M <- dbReadTable(con, "SVAL_P_NA3_M")
SVAL_P_NA3_W <- dbReadTable(con, "SVAL_P_NA3_W")

#logical check errors, example
LVAL_602 <- dbReadTable(con, "LVAL_602")

#summary
SUMMARY_SQL_PREVALID <- dbReadTable(con, gsub(" ", "",paste("SUMMARY_SQL_PREVALID_",YYYY)))


