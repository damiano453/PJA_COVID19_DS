#R project       
#Script name     :covid19_01ETL_v10.R
#Description     :This script will load data about covic19 from heterogenics sources.
#                 Transform data, and build base dataset.  
#
#
#Author		               :Roman Ch¹dzyñski
#Last modification date  :2020-04-19
#Last R_version    :R version 3.6.3
#
# v1.0 by Roman Ch¹dzyñski - initial version
# v1.1 by Roman Ch¹dzyñski - change sources for   European Center for Disease Prevention from Excel to csv

#######################################################
#
# INIT - INITIAL SECTION
#
#######################################################

#clear terminal
cat("\f")

#clear all objects
rm(list=ls())

#clear memory
gc() 

###############################
# INIT- LIBRARY SECTION

#these libraries are necessary
if(!require("utils")) install.packages("utils")
library(utils)
if(!require("data.table")) install.packages("data.table")             
library("data.table")                     
if(!require("readxl")) install.packages("readxl")
library(readxl)
if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if(!require("httr")) install.packages("httr")
library(httr)
if(!require("lubridate")) install.packages("lubridate")
library(lubridate)
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)
if(!require("jsonify")) install.packages("jsonify")
library(jsonify)
if(!require("fs")) install.packages("fs")
library(fs)
if(!require("reshape")) install.packages("reshape")
library(reshape)


###############################
# INIT- FUNCTION AND SECTION

#funkction read many excel file
read_multiple_excel <- function(path) {
  path %>%
    excel_sheets() %>% 
    set_names() %>% 
    map_df(read_excel, path = path)
}

###############################
# INIT- CONST AND GLOBAL VAR SECTION
#Exported dataset
CONST_EXPORT_DATASET <- c('df_COVID19Base', 'df_Country_DICT')

#URL - Target dataset covid19, from the European Center for Disease Prevention and Control 
CONST_ECfDP_URL <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

#files with data about population over 70
CONST_WORLDBANK_FILES <- "C:/R_project_pja/data/WorldBank/"

#files with data about country 
CONST_COUTRY_FILE <- "C:/R_project_pja/data/country.txt"

#Key for identity country 
CONST_COUNTRY_KEY <- "country"
CONST_COUNTRYID_KEY <- "countryID"
#######################################################
#
# MAIN - EXTRACT (IMPORT) DATA SECTION
#
#######################################################

###############################
# MAIN - EXTRACT DATA FROM URL the European Center for Disease Prevention and Control 
#download the dataset from the website to a local temporary file
#read the Dataset sheet into “R”. The dataset will be called "data".
data_m00 <- read.csv(CONST_ECfDP_URL, na.strings = "", fileEncoding = "UTF-8-BOM")


#change name of colummn to identity 
setnames(data_m00, "countriesAndTerritories",  CONST_COUNTRY_KEY)
setnames(data_m00, "countryterritoryCode",  CONST_COUNTRYID_KEY)


###############################
# MAIN - EXTRACT DATA FROM FILEs previusly download from WorldBank site 

#add dataSet obout population over 70 from WorldBank file 
data_df <- dir_ls(path = CONST_WORLDBANK_FILES , regexp = "xls")  %>%
map_df(read_excel, .id = "fileName")

#filter date only form filter colummn and 2018 year 
df_WordBank <- data_df[,c('fileName',"Country Name","Country Code","2018")]

#transform WorldBank dataSet to add group sex and age 
df_WordBank <- mutate(df_WordBank, sexAge= gsub(CONST_WORLDBANK_FILES,'', fileName))
df_WordBank <- mutate(df_WordBank, sexAge= gsub(".xls",'', sexAge))

#change name of colummn to identity 
setnames(df_WordBank, "Country Name",  CONST_COUNTRY_KEY)
setnames(df_WordBank, "Country Code",  CONST_COUNTRYID_KEY)

#transform WorldBank dataSet, pivot transformation
df_WordBank_PSum <- cast(df_WordBank, countryID ~ sexAge, value=2018)

#clear variables
rm(data_df)
rm(df_WordBank)

###############################
# MAIN - EXTRACT DATA FROM FILE previusly download from gitHUB site 

#add dataSet with specific info about Country 'capital','region','subregion','borders','area','latlng'
df_Country_00 <- jsonify::from_json(CONST_COUTRY_FILE)

#add (rebiuild) name of country
df_Country_00 <- cbind(df_Country_00, df_Country_00$name[1])
df_Country_00 <- cbind(df_Country_00, df_Country_00$name[2])
setnames(df_Country_00, "common",  "countryCommonName")
setnames(df_Country_00, "official", "countryOfficialName")

#rebuild vector data 'latlng' to singel data  'lat' and 'lng'
for (i in seq.int(nrow(df_Country_00))) {
  df_Country_00[i,c('lat')] <- df_Country_00$latlng[[i]][1]
  df_Country_00[i,c('lng')] <- df_Country_00$latlng[[i]][2] 
}

df_Country_01 <- df_Country_00[, c("countryCommonName", "countryOfficialName", 'cca3','capital','region','subregion','borders','area','lat','lng')]
setnames(df_Country_01, "cca3",  CONST_COUNTRYID_KEY)

#clear variables
rm(df_Country_00)

#######################################################
#
# MAIN - TRANSFORM (ENRICHMENT) DATA SECTION
#
#######################################################

#join (enrichment) base data about coronavirus with data about population from WorldBank
data_m01 <- merge(x=data_m00, y=df_WordBank_PSum, by = CONST_COUNTRYID_KEY, all=FALSE)

#transform data, add importanta colummns with agregation
data_m01 <- mutate(group_by(data_m01,country), cumsumDeaths=order_by(dateRep, cumsum(deaths)))
data_m01 <- mutate(group_by(data_m01,country), cumsumCases=order_by(dateRep, cumsum(cases)))
data_m01 <- mutate(group_by(data_m01,country), indexCases=order_by(dateRep, cumsum(ifelse(cumsumCases >0, 1, 0))))
data_m01 <- mutate(group_by(data_m01,country), indexDeaths=order_by(dateRep, cumsum(ifelse(cumsumDeaths >0, 1, 0))))
data_m01 <- mutate(group_by(data_m01,country), index100Cases=order_by(dateRep, cumsum(ifelse(cumsumCases >100, 1, 0))))
data_m01 <- mutate(group_by(data_m01,country), index10Deaths=order_by(dateRep, cumsum(ifelse(cumsumDeaths >10, 1, 0))))
data_m01 <- mutate(data_m01,indexWeekDeaths=ceiling(indexDeaths/7))
data_m01 <- mutate(data_m01,indexWeekCases=ceiling(indexCases/7))
data_m01 <- mutate(data_m01,index10WeekDeaths=ceiling(index10Deaths/7))
data_m01 <- mutate(data_m01,index100WeekCases=ceiling(index100Cases/7))
data_m01 <- mutate(data_m01,popData2018_F_70_74=ceiling(popData2018*F_70_74/100))
data_m01 <- mutate(data_m01,popData2018_M_70_74=ceiling(popData2018*M_70_74/100))
data_m01 <- mutate(data_m01,popData2018_F_75_79=ceiling(popData2018*F_75_79/100))
data_m01 <- mutate(data_m01,popData2018_M_75_79=ceiling(popData2018*M_75_79/100))
data_m01 <- mutate(data_m01,popData2018_F_80=ceiling(popData2018*F_80/100))
data_m01 <- mutate(data_m01,popData2018_M_80=ceiling(popData2018*M_80/100))
data_m01 <- mutate(group_by(data_m01, country, index10WeekDeaths),avgDeaths10DWeek=mean(as.numeric(deaths), na.rm = TRUE))
data_m01 <- mutate(group_by(data_m01, country),deathsPer1mlnAll=((deaths*(10^6))/popData2018))
data_m01 <- mutate(group_by(data_m01, country),deathsPer1mln80=((deaths*(10^6))/(popData2018_F_80+popData2018_M_80)))
data_m01 <- mutate(group_by(data_m01, country),deathsPer1mln70=((deaths*(10^6))/(popData2018_F_80+popData2018_M_80+popData2018_F_75_79+popData2018_M_75_79+popData2018_F_70_74+popData2018_M_70_74)))
data_m01 <- mutate(group_by(data_m01, country, index10WeekDeaths),avgDeaths10DWeek1mA=mean(as.numeric(deathsPer1mlnAll), na.rm = TRUE))
data_m01 <- mutate(group_by(data_m01, country, index10WeekDeaths),avgDeaths10DWeek1m80=mean(as.numeric(deathsPer1mln80), na.rm = TRUE))
data_m01 <- mutate(group_by(data_m01, country, index10WeekDeaths),avgDeaths10DWeek1m70=mean(as.numeric(deathsPer1mln70), na.rm = TRUE))
data_m01 <- mutate(data_m01,cumsumDeathsPer1mA=ceiling((cumsumDeaths*(10^6))/(popData2018)))
data_m01 <- mutate(data_m01,cumsumCasesPer1mA=ceiling((cumsumCases*(10^6))/(popData2018)))
data_m01 <- mutate(data_m01,cumsumDeathsPer1m70=ceiling((cumsumDeaths*(10^6))/(popData2018_F_80+popData2018_M_80+popData2018_F_75_79+popData2018_M_75_79+popData2018_F_70_74+popData2018_M_70_74)))
data_m01 <- mutate(data_m01,cumsumCasesPer1m70=ceiling((cumsumCases*(10^6))/(popData2018_F_80+popData2018_M_80+popData2018_F_75_79+popData2018_M_75_79+popData2018_F_70_74+popData2018_M_70_74)))

#add data from previus and next term
danePrevNext <- data_m01[which((data_m01$index10Deaths%%7)==1),c(CONST_COUNTRYID_KEY,'index10WeekDeaths',"avgDeaths10DWeek")]

danePrevNext <- mutate(danePrevNext,index10WeekDeathsPrv=index10WeekDeaths+1)
danePrevNext <- mutate(danePrevNext,avgDeaths10DWeekPrv=avgDeaths10DWeek)
danePrevNext <- mutate(danePrevNext,index10WeekDeathsNext=index10WeekDeaths-1)
danePrevNext <- mutate(danePrevNext,avgDeaths10DWeekNext=avgDeaths10DWeek)

data_m01 <- merge(x=data_m01, y=danePrevNext[,c(CONST_COUNTRYID_KEY, 'index10WeekDeathsPrv','avgDeaths10DWeekPrv')], by.x = c(CONST_COUNTRYID_KEY, 'index10WeekDeaths'), by.y = c(CONST_COUNTRYID_KEY, 'index10WeekDeathsPrv'), all.x = TRUE)
data_m01 <- merge(x=data_m01, y=danePrevNext[,c(CONST_COUNTRYID_KEY, 'index10WeekDeathsNext','avgDeaths10DWeekNext')], by.x = c(CONST_COUNTRYID_KEY, 'index10WeekDeaths'), by.y = c(CONST_COUNTRYID_KEY, 'index10WeekDeathsNext'), all.x = TRUE)
data_m01[["avgDeaths10DWeekPrv"]][is.na(data_m01[["avgDeaths10DWeekPrv"]])] <- 0


#join (enrichment) data with data about country
data_m02 <- merge(x=data_m01, y=df_Country_01, by = CONST_COUNTRYID_KEY, all=FALSE)

##dateRep has to by not null 
data_m02 <- data_m02[!is.na(data_m02$dateRep),]
data_m02 <- data_m02[!is.na(data_m02$countryID),]

#create dict dataSet with all country (name, country_ID)
df_Country_DICT <- data_m02[, c("countryCommonName", "countryOfficialName", 'countryID','country')]
#UNIQ POSITION
df_Country_DICT <- unique(df_Country_DICT)
#coutryID has to by not null 
df_Country_DICT <- df_Country_DICT[!is.na(df_Country_DICT$countryID),]
#one name of country has to by not null 
df_Country_DICT <- df_Country_DICT[(!is.na(df_Country_DICT$countryCommonName))|(!is.na(df_Country_DICT$countryOfficialName))|(!is.na(df_Country_DICT$country)),]

#clear variables
rm(data_m00)
rm(df_WordBank_PSum)
rm(danePrevNext)
rm(data_m01)
rm(df_Country_01)

#######################################################
#
# MAIN - LOAD (EXPORT) DATA SECTION
#
#######################################################
# lexport data to databas or other packages
df_COVID19Base <- data_m02
df_Country_DICT <-df_Country_DICT

#######################################################
#
# FINAL SECTION
#
#######################################################
#Remove all variables without CAST_EXPORT_DATASET
ls_VariablesToRm <- NULL
ls_VariablesToRm <- ls()
ls_VariablesToRm <-ls_VariablesToRm[! ls_VariablesToRm %in% CONST_EXPORT_DATASET]
rm(list=ls_VariablesToRm)
gc() 
