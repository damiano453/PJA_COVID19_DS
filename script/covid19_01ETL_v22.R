#R project       
#Script name     :covid19_01ETL_v10.R
#Description     :This script will load data about covic19 from heterogenics sources.
#                 Transform data, and build base dataset.  
#
#
#Author		               :Roman Ch?dzy?ski
#Last modification date  :2020-04-19
#Last R_version    :R version 3.6.3
#
# v1.0 by Roman Ch?dzy?ski - initial version
# v1.1 by Roman Ch?dzy?ski - change sources for   European Center for Disease Prevention from Excel to csv
# v2.0 by Roman Ch?dzy?ski - change sources for   European Center for Disease Prevention from Excel to csv

#TODO:
#add comparative data flu WHO
#add comparative data pneumonia WHO
#save date into R format file  
#save date into csv format file  
#save date into parquet format file  
#save date into json format file  
#save date into database
#separte script with ETL data about country (create country DICT)

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
if(!require("fs")) install.packages("fs")
library(fs)
if(!require("zoo")) install.packages("zoo")             
library(zoo)                  
if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)
if(!require("data.table")) install.packages("data.table")
library(data.table)
if(!require("httr")) install.packages("httr")
library(httr)
if(!require("lubridate")) install.packages("lubridate")
library(lubridate)
if(!require("readxl")) install.packages("readxl")
library(readxl)
if(!require("jsonify")) install.packages("jsonify")
library(jsonify)
if(!require("reshape")) install.packages("reshape")
library(reshape)

###############################
# INIT- FUNCTION AND SECTION

#function for create colection from siutable countries
f_collapseColumns2Colection <- function(df_add, key, nameColectColumn,nameNewColumn ){
  data_colection <- df_add %>%
    group_by_at(key) %>% 
    summarise_at(nameColectColumn,list)
  
  setnames( data_colection, nameColectColumn, nameNewColumn)
  
  df_add <- merge(x=df_add, y=data_colection, by = key , all.x=TRUE)
  return(df_add)
}

#function for registration columns for finall data set
f_orderColumnFinallDataSet <- function(nameOfVector){
  
  V_key       <- c("PK_countryID", "PK_dateReport1DayNatural")  
  v_country  <- c("countryOfficialName", "countryCommonName", "capital", "area", "countryGeoID" , "c_latlng", "lat", "lng", "c_neighbors", "region", "c_region", "subregion", "c_subregion", "c_groupDistance300","c_groupDistance500","c_groupDistance1000","independent")
  
  v_casesandaggregation <- c("cases1DayNatural","mmeanCases1DayNatural", "cumSumCases1DayNatural", 
                             "sumCases7DayNaturalPrev7Day", "sumCases7DayNatural", "sumCases7DayNaturalNext7Day", "sumCases1MonthNatural", 
                             "avgCases7DayNaturalPrev7Day", "avgCases7DayNatural", "avgCases7DayNaturalNext7Day", "avgCases1MonthNatural",
                             "quanCases7DayNaturalPrev7Day", "quanCases7DayNatural", "quanCases7DayNaturalNext7Day", "quanCases1MonthNatural")    
  
  v_deathsandaggregation <- c("deaths1DayNatural", "mmeanDeath1DayNatural", "cumSumDeath1DayNatural",
                              "sumDeaths7DayNaturalPrev7Day", "sumDeaths7DayNatural", "sumDeaths7DayNaturalNext7Day", "sumDeaths1MonthNatural",
                              "avgDeaths7DayNaturalPrev7Day", "avgDeaths7DayNatural", "avgDeaths7DayNaturalNext7Day", "avgDeaths1MonthNatural",
                              "quanDeaths7DayNaturalPrev7Day",  "quanDeaths7DayNatural", "quanDeaths7DayNaturalNext7Day", "quanDeaths1MonthNatural")
  
  V_population   <- c("population2018_A", 
                      "population2018_A_70", "population2018_F_70","population2018_M_70",  
                      "population2018_A_80", "population2018_F_80", "population2018_M_80")
  v_otherIndicator <-c("GDP_US")
  
  V_indexNatural <-  c("index1DayNatural",  "index7DayNatural", "index1MonthNatural", "index1YearNatural")  
  V_indexCases   <-  c("index1Day1Case", "index7Day1Case", "index1Day10cumSumCaseP100t_A_70", "index1Day10cumSumCaseP1mAll",   "index7Day10cumSumCaseP100t_A_70", "index7Day10cumSumCaseP1mAll")                    
  V_indexDeath   <-  c("index1Day1Death", "index7Day1Death", "index1Day1cumSumDeathP100t_A_70", "index1Day1cumSumDeathP1mAll", "index7Day1cumSumDeathP100t_A_70", "index7Day1cumSumDeathP1mAll")   
  
  v_country_SUM <- c("PK_countryID", v_country, V_population,v_otherIndicator )
  v_order_column_SUM <- c(V_key,v_country, v_casesandaggregation, v_deathsandaggregation, V_population,  v_otherIndicator, V_indexNatural, V_indexCases, V_indexDeath)
  
  return(get(nameOfVector))
}

###############################
# INIT- CONST AND GLOBAL VAR SECTION

#URL - Target dataset covid19, from the European Center for Disease Prevention and Control 
CONST_ECfDP_URL <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

#files with data about population over 70
CONST_WORLDBANK_FILES <- "../data/WorldBank/"
CONST_WORLDBANK_FILES_INDICATORS <- paste0(CONST_WORLDBANK_FILES,"indicators/") 

#files with data about country distance
CONST_COUTRY_DISTANCE_FILE <- "../data/countries_distances.csv"

#files with data about country 
CONST_COUTRY_FILE <- "../data/country.txt"

#key for identity country 
CONST_COUNTRY_KEY <- "country"
CONST_COUNTRYID_KEY <- "countryID"

#period form grouping and moving means  
CONST_BASE_PERIOD <- 7

#exported dataset
CONST_EXPORT_DATASET <- c('df_COVID19Base', 'df_Country_DICT')

#######################################################
#
# MAIN (STAGE AREA) - EXTRACT (IMPORT) DATA SECTION
#
#######################################################


###############################
# MAIN (STAGE AREA) - EXTRACT MAIN COVID 19 SET FROM URL the European Center for Disease Prevention and Control 
#download the dataset from the website to a local temporary file
#read the Dataset sheet into ?R?. The dataset will be called "data".
data_m00 <- read.csv(CONST_ECfDP_URL, na.strings = "", fileEncoding = "UTF-8-BOM")

#change name of colummn to identity 
setnames(data_m00, "countriesAndTerritories",  CONST_COUNTRY_KEY)
setnames(data_m00, "countryterritoryCode",  CONST_COUNTRYID_KEY)

###############################
# MAIN (STAGE AREA) - EXTRACT INDICATOR SET (POPOULATION, GDP) FROM FILEs previusly download from WorldBank site 

#add dataSet obout from WorldBank file 
data_df <- dir_ls(path = CONST_WORLDBANK_FILES , regexp = ".xls")  %>%
map_df(read_excel, .id = "fileName")

#filter date only form filter colummn and 2018 year 
df_WordBank <- data_df[,c('fileName',"Country Name","Country Code","VALUE")]

#transform WorldBank dataSet to add group sex and age 
df_WordBank <- mutate(df_WordBank, indicatorName= gsub(CONST_WORLDBANK_FILES,'', fileName))
df_WordBank <- mutate(df_WordBank, indicatorName= gsub(".xls",'', indicatorName))

#change name of colummn to identity 
setnames(df_WordBank, "Country Name",  CONST_COUNTRY_KEY)
setnames(df_WordBank, "Country Code",  CONST_COUNTRYID_KEY)

#transform WorldBank dataSet, pivot transformation
df_WordBank_PSum <- cast(df_WordBank, countryID ~ indicatorName, value="VALUE")

#clear variables
rm(data_df)
rm(df_WordBank)

###############################
# MAIN (STAGE AREA) - EXTRACT COUNTRY SET FROM FILE previusly download from gitHUB site 

#add dataSet with specific info about Country 'capital','region','subregion','borders','area','latlng'
df_Country_00 <- jsonify::from_json(CONST_COUTRY_FILE)

#change name of column with country_id (key to join) 
setnames(df_Country_00, "cca3",  CONST_COUNTRYID_KEY)
setnames(df_Country_00, "cca2",  "countryGeoID")

#change name of column with colection latlng
setnames(df_Country_00, "latlng",  paste0('c_', 'latlng'))

#change column name with neighbors grup
setnames(df_Country_00, "borders", "c_neighbors"  )

#add (rebiuild) name of country
df_Country_00 <- cbind(df_Country_00, df_Country_00$name[1])
df_Country_00 <- cbind(df_Country_00, df_Country_00$name[2])
setnames(df_Country_00, "common",  "countryCommonName")
setnames(df_Country_00, "official", "countryOfficialName")

#rebuild vector data 'latlng' to singel data'lat' and 'lng'
for (i in seq.int(nrow(df_Country_00))) {
  df_Country_00[i,c('lat')] <- df_Country_00$c_latlng[[i]][1]
  df_Country_00[i,c('lng')] <- df_Country_00$c_latlng[[i]][2] 
}

#add new colection column c_region, c_subregion 
df_Country_00 <- f_collapseColumns2Colection(df_Country_00, 'region' , CONST_COUNTRYID_KEY, 'c_region')
df_Country_00 <- f_collapseColumns2Colection(df_Country_00, 'subregion' , CONST_COUNTRYID_KEY, 'c_subregion')

#add colunm to agregate all data
df_Country_00$allGroup <- "All"


#zoatawiem c lat i zminiamy na c border
df_Country_01 <- df_Country_00[, c("countryID",  "countryGeoID",             
                                   "countryCommonName", "countryOfficialName",                                 
                                   "independent",
                                   "capital",
                                   "c_neighbors",
                                   "area",                         
                                   "c_latlng" ,"lat", "lng",                 
                                   "region", "c_region",           
                                   "subregion","c_subregion",
                                   "allGroup" )]

#clear variables
rm(df_Country_00)

###############################
#MAIN (STAGE AREA) - EXTRACT DISTANCE_COUNTRES SET FROM FILE previusly download from gitHUB site 
#load file with distance 
df_CountryDistance_00 <- read.csv(CONST_COUTRY_DISTANCE_FILE, na.strings = "", fileEncoding = "UTF-8-BOM")

#load file with countryID 
df_CountryDistance_DICT <- df_Country_01[,c("countryID", "countryCommonName", "countryOfficialName")]

#join (enrichment) add countryID for left side of distance relation
df_CountryDistance_01 <- merge(x=df_CountryDistance_00, y=df_CountryDistance_DICT, by.x = 'pays1', by.y = 'countryCommonName' , all.x=TRUE) 
setnames(df_CountryDistance_01, "countryID",  "countryID_01")
df_CountryDistance_01 <- merge(x=df_CountryDistance_01, y=df_CountryDistance_DICT, by.x = 'pays1', by.y = 'countryOfficialName' , all.x=TRUE)
setnames(df_CountryDistance_01, "countryID",  "countryID_02")

df_CountryDistance_01 <- df_CountryDistance_01[, c('pays1', 'pays2', 'dist', 'countryID_01','countryID_02')]
df_CountryDistance_01 <- unite(df_CountryDistance_01, 'countryID_LEFT' , c('countryID_01','countryID_02'))
df_CountryDistance_01$countryID_LEFT <- gsub('NA_', '', df_CountryDistance_01$countryID_LEFT)
df_CountryDistance_01$countryID_LEFT <- substr(df_CountryDistance_01$countryID_LEFT,1,3)

#coutryID exeption
df_CountryDistance_01[df_CountryDistance_01$pays1=="Macedonia",]$countryID_LEFT <- "MKD"
df_CountryDistance_01[df_CountryDistance_01$pays1=="Tobago",]$countryID_LEFT <- "TTO"
df_CountryDistance_01[df_CountryDistance_01$pays1=="UK",]$countryID_LEFT <- "GBR"
df_CountryDistance_01[df_CountryDistance_01$pays1=="USA",]$countryID_LEFT <- "USA"

#join (enrichment) add countryID for right side of distance relation
df_CountryDistance_01_RIGHT<- unique(df_CountryDistance_01[,c('pays1', 'countryID_LEFT')])
setnames(df_CountryDistance_01_RIGHT, "countryID_LEFT",  "countryID_RIGHT")
df_CountryDistance_02 <- merge(x=df_CountryDistance_01, y=df_CountryDistance_01_RIGHT, by.x = 'pays2', by.y = 'pays1' , all.x=TRUE) 

#finall distance set
setnames(df_CountryDistance_02, "pays1",  "countryName_LEFT")
setnames(df_CountryDistance_02, "pays2",  "countryName_RIGHT")

#coutryID REJECT
df_CountryDistance_REJECT<- df_CountryDistance_02[(df_CountryDistance_02$countryID_LEFT=='NA')|(df_CountryDistance_02$countryID_RIGHT=='NA'),]
df_CountryDistance_02 <- df_CountryDistance_02[(df_CountryDistance_02$countryID_LEFT!='NA')&(df_CountryDistance_02$countryID_RIGHT!='NA'),]


for (x_distance in c(300,500,1000)) {
df_CountryDistance_02_01 <- mutate(df_CountryDistance_02, groupDistance=ifelse(dist < x_distance, paste0(x_distance,countryID_LEFT),0))
df_CountryDistance_02_01 <- df_CountryDistance_02_01[df_CountryDistance_02_01$groupDistance!=0,]
df_CountryDistance_02_01 <- f_collapseColumns2Colection(df_CountryDistance_02_01, "groupDistance" ,"countryID_RIGHT", "c_groupDistance")
df_CountryDistance_02_01 <- unique(df_CountryDistance_02_01[,c("countryID_LEFT","c_groupDistance")])
df_CountryDistance_DICT <- merge(x=df_CountryDistance_DICT, y=df_CountryDistance_02_01, by.x = 'countryID', by.y = 'countryID_LEFT' , all.x=TRUE) 
setnames(df_CountryDistance_DICT, "c_groupDistance",  paste0("c_groupDistance", x_distance)) 
}
df_CountryDistance_DICT <- df_CountryDistance_DICT %>% select(-matches("Name$"))
#clear variables
rm(df_CountryDistance_00)
rm(df_CountryDistance_01)
rm(df_CountryDistance_01_RIGHT)
rm(df_CountryDistance_02)
rm(df_CountryDistance_02_01)

#######################################################
#
# MAIN - TRANSFORM (ENRICHMENT) DATA SECTION
#
#######################################################

#TRANSFORM JOIN country set with distance set
df_Country_10 <- df_Country_01
rm(df_Country_01)

#TRANSFORM JOIN country set with data about population from WorldBank
df_Country_11 <- merge(x=df_Country_10, y=df_WordBank_PSum, by = CONST_COUNTRYID_KEY, all=FALSE)
rm(df_WordBank_PSum)
rm(df_Country_10)

#TRANSFORM JOIN country set with data about population from WorldBank
df_Country_12 <- merge(x=df_Country_11, y=df_CountryDistance_DICT, by = CONST_COUNTRYID_KEY, all=FALSE)
rm(df_CountryDistance_DICT)
rm(df_Country_11)

#JOIN base data about coronavirus with data about country
data_m01 <- merge(x=data_m00, y=df_Country_12, by = CONST_COUNTRYID_KEY, all.x=TRUE)
rm(data_m00)

##TRANSFORM dateRep is not null and coutry_id is not null
data_m01_REJECT <- data_m01[is.na(data_m01$dateRep) | is.na(data_m01$countryID) | is.na(data_m01$countryOfficialName),]
data_m01 <- data_m01[!is.na(data_m01[,'dateRep']),]
data_m01 <- data_m01[!is.na(data_m01[,CONST_COUNTRYID_KEY]),]

#TRANSFORM remove not use column
data_m01$continentExp <- NULL
data_m01$geoId <- NULL

#TRANSFORM population data to numeric
setnames(data_m01, "popData2018",  "population2018_A")
data_m01$population2018_A <-as.numeric(data_m01$population2018_A) 

#TRANSFORM age group over 80 (M- male, F-fmale)
data_m01 <- mutate(data_m01,population2018_F_80=ceiling(population2018_A*F_80/100))
data_m01 <- mutate(data_m01,population2018_M_80=ceiling(population2018_A*M_80/100))
data_m01 <- mutate(data_m01,population2018_A_80=population2018_F_80+population2018_M_80)

#TRANSFORM age group over 70
data_m01 <- mutate(data_m01,population2018_F_70=ceiling(population2018_A * (F_70_74+F_75_79)/100)+population2018_F_80)
data_m01 <- mutate(data_m01,population2018_M_70=ceiling(population2018_A * (M_70_74+M_75_79)/100)+population2018_M_80)
data_m01 <- mutate(data_m01,population2018_A_70=population2018_F_70+population2018_M_70)

#TRANSFORM remove not need columns
data_m01 <- data_m01 %>% select(-matches("^F_"))
data_m01 <- data_m01 %>% select(-matches("^M_"))

#TRANSFORM transform string date raporting to date format
data_m01 <- mutate(data_m01, dateReport1DayNatural = as.Date(dateRep, "%d/%m/%Y"))
#data_m01$dateReport1DayNatural <- as.Date(data_m01$dateReport1DayNatural) 
data_m01$dateRep <-NULL

#TRANSFORM  first indexys natural
#days index
data_m01$index1DayNatural <- yday(data_m01$dateReport1DayNatural)
#week index
data_m01$index7DayNatural <- week(data_m01$dateReport1DayNatural)
#month index
data_m01$index1MonthNatural <- month(data_m01$dateReport1DayNatural)
#month index
data_m01$index1YearNatural <- year(data_m01$dateReport1DayNatural)
data_m01$day <-NULL
data_m01$month <-NULL
data_m01$year <-NULL

#TRANSFORM add culative sum of death and cases 
#change name of death column
setnames(data_m01, "deaths", "deaths1DayNatural")
setnames(data_m01, "cases", "cases1DayNatural")
data_m01$deaths1DayNatural <- as.numeric(data_m01$deaths1DayNatural)
data_m01$cases1DayNatural <- as.numeric(data_m01$cases1DayNatural)

#TRANSFORM add column with cumulativ sum of death 
data_m01 <- mutate(group_by_at(data_m01,CONST_COUNTRYID_KEY), cumSumDeath1DayNatural=order_by(dateReport1DayNatural, cumsum(deaths1DayNatural)))
#add column with roll mean of death 
data_m01 <- data_m01 %>% mutate(mmeanDeath1DayNatural=order_by(dateReport1DayNatural, rollmean(deaths1DayNatural, k = CONST_BASE_PERIOD, fill = 0, align = "right")))
#add column with summary,avg, quantile of death during week
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index7DayNatural")), sumDeaths7DayNatural = sum(deaths1DayNatural, na.rm = TRUE))
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index7DayNatural")), avgDeaths7DayNatural = mean(deaths1DayNatural, na.rm = TRUE))
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index7DayNatural")), quanDeaths7DayNatural = list(quantile(deaths1DayNatural,type = 1, na.rm = TRUE)))
#add column with summary,avg, quantile of death during month
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index1MonthNatural")), sumDeaths1MonthNatural = sum(deaths1DayNatural, na.rm = TRUE))
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index1MonthNatural")), avgDeaths1MonthNatural = mean(deaths1DayNatural, na.rm = TRUE))
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index1MonthNatural")), quanDeaths1MonthNatural = list(quantile(deaths1DayNatural,type = 1, na.rm = TRUE)))

#TRANSFORM  add column with cumulativ sum of cases
data_m01 <- mutate(group_by_at(data_m01,CONST_COUNTRYID_KEY), cumSumCases1DayNatural=order_by(dateReport1DayNatural, cumsum(cases1DayNatural)))
#add column with roll mean of cases
data_m01 <- data_m01 %>% mutate(mmeanCases1DayNatural=order_by(dateReport1DayNatural, rollmean(cases1DayNatural, k = CONST_BASE_PERIOD, fill = 0, align = "right")))
#add column with summary,avg, quantile of cases during week
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index7DayNatural")), sumCases7DayNatural = sum(cases1DayNatural, na.rm = TRUE))
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index7DayNatural")), avgCases7DayNatural = round(mean(cases1DayNatural, na.rm = TRUE)))
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index7DayNatural")), quanCases7DayNatural = list(quantile(cases1DayNatural,type = 1, na.rm = TRUE)))
#add column with summary,avg, quantile of cases during month
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index1MonthNatural")), sumCases1MonthNatural = sum(cases1DayNatural, na.rm = TRUE))
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index1MonthNatural")), avgCases1MonthNatural = round(mean(cases1DayNatural, na.rm = TRUE)))
data_m01 <- mutate(group_by_at(data_m01, c(CONST_COUNTRYID_KEY, "index1MonthNatural")), quanCases1MonthNatural = list(quantile(cases1DayNatural,type = 1, na.rm = TRUE)))


#TRANSFORM  add indexes (how many days is after the event)
data_m01 <- mutate(group_by(data_m01,country), index1Day1Death=order_by(dateReport1DayNatural, cumsum(ifelse(cumSumDeath1DayNatural>0, 1, 0))))
data_m01 <- mutate(group_by(data_m01,country), index1Day1Case=order_by(dateReport1DayNatural, cumsum(ifelse( cumSumCases1DayNatural >0, 1, 0))))

data_m01 <- mutate(group_by(data_m01,country), index1Day1cumSumDeathP1mAll=order_by(dateReport1DayNatural, cumsum(ifelse(cumSumDeath1DayNatural >(1*(population2018_A/(10^6))), 1, 0))))
data_m01 <- mutate(group_by(data_m01,country), index1Day10cumSumCaseP1mAll=order_by(dateReport1DayNatural, cumsum(ifelse( cumSumCases1DayNatural >(10*(population2018_A/(10^6))), 1, 0))))

data_m01 <- mutate(group_by(data_m01,country), index1Day1cumSumDeathP100t_A_70=order_by(dateReport1DayNatural, cumsum(ifelse(cumSumDeath1DayNatural >(1*(population2018_A_70/(10^5))), 1, 0))))
data_m01 <- mutate(group_by(data_m01,country), index1Day10cumSumCaseP100t_A_70=order_by(dateReport1DayNatural, cumsum(ifelse( cumSumCases1DayNatural >(10*(population2018_A_70/(10^5))), 1, 0))))


#TRANSFORM  find natural weekend siutable to first day of index (group of index)
v_Indexes_Week = c("index1Day1Death", "index1Day1Case", "index1Day1cumSumDeathP1mAll", "index1Day10cumSumCaseP1mAll", "index1Day1cumSumDeathP100t_A_70", "index1Day10cumSumCaseP100t_A_70")
for ( x_indexName in v_Indexes_Week) {
  # select data when value of index is equel 1, and siutable for dzis date natural week number of week 
  df_week_1 <- data_m01[which((data_m01[,x_indexName])==1),c(CONST_COUNTRYID_KEY,"index7DayNatural")]
  setnames(df_week_1, "index7DayNatural",  "week_1")
  #number of siutble natural week decrease (- 1), it is value which will by use to counting number of weekend 
  df_week_1['week_1'] <- df_week_1$week_1 - 1 
  #counting siutable weekend to the index
  data_m01 <- merge(x=data_m01, y=df_week_1, by = CONST_COUNTRYID_KEY, all=FALSE)
  x_indexWeekName <- str_replace(x_indexName, "1Day", "7Day")
  data_m01[x_indexWeekName] <- data_m01["index7DayNatural"] - data_m01["week_1"] 
  #all weekend befor event is equal 0
  data_m01[data_m01[,x_indexWeekName]<1,x_indexWeekName] <- 0
  #drop technical column
  data_m01$week_1 <- NULL
}

#TRANSFORM add data from previus and next term (7day)
#select data for first day of week
CONST_Aggregat_Column_Death_Week <- c("sumCases7DayNatural", "avgCases7DayNatural", "quanCases7DayNatural")
CONST_Aggregat_Column_Cases_Week <- c("sumDeaths7DayNatural", "avgDeaths7DayNatural", "quanDeaths7DayNatural")
data_PrevNext7Day <- data_m01[which((data_m01$index1DayNatural%%7)==1),c(CONST_COUNTRYID_KEY,"index7DayNatural",CONST_Aggregat_Column_Death_Week,CONST_Aggregat_Column_Cases_Week)]

#internal function to marge pivius and next term
mergePriVNextTermColumns <- function(data_add, columnNameSufix, yIndex2Merge ){
  for (v_columnName in c(CONST_Aggregat_Column_Death_Week, CONST_Aggregat_Column_Cases_Week)) {
      setnames(data_add, v_columnName ,  paste0(v_columnName, columnNameSufix))
      }
  data_add$index7DayNatural <- NULL

  #merge previus term data
  data_m01 <- merge(x=data_m01, y=data_add, by.x = c(CONST_COUNTRYID_KEY, 'index1DayNatural'), by.y = c(CONST_COUNTRYID_KEY, yIndex2Merge), all.x = TRUE)
  return(data_m01)
}

#change index up for merging privius term
data_Prev7Day <- mutate(data_PrevNext7Day,index7DayNaturalPrev=index7DayNatural+1)
data_m01 <- mergePriVNextTermColumns(data_Prev7Day, "Prev7Day", "index7DayNaturalPrev")

#change index down form merging next term
data_Next7Day <- mutate(data_PrevNext7Day,index7DayNaturalNext=index7DayNatural-1)
data_m01<- mergePriVNextTermColumns(data_Next7Day, "Next7Day", "index7DayNaturalNext")

#clear variables
rm(data_PrevNext7Day)
rm(data_Prev7Day)
rm(data_Next7Day)



#######################################################
#
# MAIN - LOAD (EXPORT) DATA SECTION
#
#######################################################

#generate order of colums for finall data set 
#drop not used columns
data_m01$allGroup <- NULL 
data_m01$country <-NULL
setnames(data_m01, "countryID", "PK_countryID")
setnames(data_m01, "dateReport1DayNatural", "PK_dateReport1DayNatural")

#test column for finall order (te registered column are siutable to real column) 
columnsRegisterFALSE_data_m02_REJECT <- setdiff(f_orderColumnFinallDataSet("v_order_column_SUM"), colnames(data_m01))
                             
#finall order columns in data set
data_m02 <- data_m01[ ,f_orderColumnFinallDataSet("v_order_column_SUM")]                              

#create dict dataSet with all country 
df_Country_DICT <- data_m02[,  c(f_orderColumnFinallDataSet("v_country_SUM"))]
#only UNIQue 
df_Country_DICT <- unique(df_Country_DICT)
#PK_coutryID has to by not null 
df_Country_DICT <- df_Country_DICT[!is.na(df_Country_DICT$PK_countryID),]
#one name of country has to by not null 
df_Country_DICT <- df_Country_DICT[(!is.na(df_Country_DICT$countryCommonName))|(!is.na(df_Country_DICT$countryOfficialName)),]

#clear variables
rm(data_m01)


# export data to databas or other packages
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
ls_VariablesToRm <-ls_VariablesToRm[!ls_VariablesToRm %in% ls_VariablesToRm[(ls_VariablesToRm %in% CONST_EXPORT_DATASET) | (ls_VariablesToRm %like% '*_REJECT')]]
rm(list=ls_VariablesToRm)
gc()
