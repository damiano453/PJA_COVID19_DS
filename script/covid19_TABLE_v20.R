#R project       
#Script name     :covid19_TABLE_v20.R
#Description     :This script will create html TABLE with data.
#                 Base dataSet is covid19_Base and df_Country_DICT   
#
#
#Author		               :Roman Ch¹dzyñski
#Last modification date  :2020-04-19
#Last R_version          :R version 3.6.3
#
# v1.0 by Roman Ch¹dzyñski - initial version
# v2.0 by Roman Ch¹dzyñski - final version

#######################################################
#
# INIT - INITIAL SECTION
#
#######################################################

#clear terminal
cat("\f")

#Remove all variables without CONST_IMPORT_DATASET
CONST_IMPORT_DATASET <- c('df_COVID19Base', 'df_Country_DICT', 'CONST_IMPORT_DATASET')
ls_VariablesToRm <- NULL
ls_VariablesToRm <- ls()
ls_VariablesToRm <-ls_VariablesToRm[! ls_VariablesToRm %in% CONST_IMPORT_DATASET]
rm(list=ls_VariablesToRm)

#clear memory
gc() 

###############################
# INIT- LIBRARY SECTION

#these libraries are necessary
if(!require("reshape")) install.packages("reshape")
library(reshape)

if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("DT")) install.packages("DT")
library(DT)


###############################
# INIT- FUNCTION AND SECTION

#function for registration columns for finall data set
f_orderColumnFinallDataSet <- function(nameOfVector){
 
  v_normal_table_base    <- c("countryCommonName", "PK_dateReport1DayNatural","index1Day1cumSumDeathP1mAll","index7Day1cumSumDeathP1mAll",
                              "cumSumDeath1DayNatural", "cumSumCases1DayNatural", "population2018_A_70", "population2018_A")
  
  v_normal_table_result  <- c("countryCommonName", "date1Day1cumSumDeathP1mAll" , "index7Day1cumSumDeathP1mAll",
                              "cumSumDeath1DayNaturalP1mAll", "cumSumDeath1DayNaturalP100t_A_70","cumSumDeath1DayNatural", "cumSumCases1DayNatural")
  
  v_normal_table_columns <- c("country", "first day", "week after\nfirst day", 
                              "totalSum\nDeath\nperCapita", "totalSum\nDeath\nperCapita70", "totalSum\nDeaths",  "totalSum\nCases")
  
  v_big_table_base      <- c("PK_countryID", "PK_dateReport1DayNatural","countryCommonName",
                        "deaths1DayNatural","cases1DayNatural",
                        "cumSumDeath1DayNatural",  "cumSumCases1DayNatural", 
                        "index1Day1cumSumDeathP1mAll", "index7Day1cumSumDeathP1mAll", 
                        "index1Day10cumSumCaseP1mAll", "index7Day1cumSumDeathP1mAll" , 
                        "avgDeaths7DayNaturalPrev7Day", "avgDeaths7DayNatural", "avgDeaths7DayNaturalNext7Day",
                        "population2018_A_70", "population2018_A")
  
  v_big_table_result <- v_big_table_base
  
  v_big_table_columns     <- c("countryID", "dateReport","country", 
                               "index\nDeaths\n1Day","Index\nCases\n1Day",
                               "totalSum\nDeath",  "totalSum\nCases", 
                               "index\nTotalDeath\n1P1mAll", "indexWeek\nTotalDeath\nP1mAll", 
                               "index\nTotalCase\n10P1mAll", "indexWeek\nTotalDeath\nP1mAll" , 
                               "avgDeaths\nWeek\nPrevius", "avgDeaths\nWeek\nCurrent", "avgDeaths\nWeek\nNext",
                               "population\nover_70", "population")
  return(get(nameOfVector))
}

#function to return value from dictionary
lookupDict <-function(dictionary, pk_dict, pk_value,  column_out){
  value_out <- dictionary %>% filter_at(vars(pk_dict), all_vars(. == pk_value)) %>% select(c(column_out))
  return(as.character(value_out))
}

###############################
# INIT- CONST AND GLOBAL VAR SECTION

#Exported dataset
CONST_EXPORT_DATASET <- CONST_IMPORT_DATASET

#path to export heat map
CONST_EXPORT_PATH <- "C:/R_project_pja/table/"

#Key for identity country 
CONST_COUNTRYID_KEY <- "PK_countryID"


#Which tabele is generate 
#1- Normal table 
#2- Big table
CONST_TABLE_TYP <-2

#Typ of product
CONST_NAME_OF_PRODUCT_TYP <- as.factor(c('normaltable', 'bigtable'))

#Type of selection
#1. list of selection is from country record (name of product is cod of coutry plus sufix)
CONST_NAME_OF_PRODUCT <- 1
CONST_DATE_OF_PRODUCT <- '15/06/2020'
CONST_NAME_OF_PRODUCT_PREFIX <- ''
CONST_NAME_OF_PRODUCT_SUFIX <- '_country'
m_FieldFilterValue <-  "PK_countryID"

# CONST_NAME_OF_PRODUCT <- 1
# CONST_DATE_OF_PRODUCT <- '15/06/2020'
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_DIST1000'
# m_FieldFilterValue <- 'c_groupDistance1000'

# CONST_NAME_OF_PRODUCT <- 1
# CONST_DATE_OF_PRODUCT <- '15/06/2020'
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_DIST500'
# m_FieldFilterValue <- 'c_groupDistance500'

# CONST_NAME_OF_PRODUCT <- 1
# CONST_DATE_OF_PRODUCT <- '15/06/2020'
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_DIST300'
# m_FieldFilterValue <- 'c_groupDistance300'

# CONST_NAME_OF_PRODUCT <- 1
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_neighbors'
# m_FieldFilterValue <-  "c_neighbors"

# CONST_NAME_OF_PRODUCT <- 1
# CONST_DATE_OF_PRODUCT <- '15/06/2020'
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_region'
# m_FieldFilterValue <- "c_region"

# CONST_NAME_OF_PRODUCT <- 1
# CONST_DATE_OF_PRODUCT <- '15/06/2020'
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_subregio'
# m_FieldFilterValue <- "c_subregion"

#2. list of selection is static (is generat for static category)
# CONST_VECTOR_NAME_OF_PRODUCT contain list with NAME of Category and list of country which is form filternig
# # region
# CONST_NAME_OF_PRODUCT <-2
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_region'
# CONST_VECTOR_NAME_OF_PRODUCT <-  df_Country_DICT %>% select (region, c_region) %>% distinct_all()

# # subregion
# CONST_NAME_OF_PRODUCT <-2
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_subregion'
# CONST_VECTOR_NAME_OF_PRODUCT <- df_Country_DICT %>% select (subregion, c_subregion) %>% distinct_all()

# # top20deaths
# CONST_NAME_OF_PRODUCT <-2
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- ''
# CONST_VECTOR_NAME_OF_PRODUCT <- df_COVID19Base %>%
#                                 select (PK_countryID,cumSumDeath1DayNatural) %>%
#                                 group_by(PK_countryID) %>% summarise(maxOservation = max(cumSumDeath1DayNatural))  %>%
#                                 arrange(desc(maxOservation)) %>% head(20) %>%
#                                 select (PK_countryID)  %>% mutate(AAA="top20deaths")  %>%
#                                 group_by(AAA) %>% summarise_at("PK_countryID",list)
 
# # top20people   
# CONST_NAME_OF_PRODUCT <-2
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- ''
# CONST_VECTOR_NAME_OF_PRODUCT <- df_Country_DICT %>%
#                                 select (PK_countryID,population2018_A) %>%
#                                 arrange(desc(population2018_A)) %>% head(20) %>%
#                                 select (PK_countryID)  %>% mutate(AAA="top20people")  %>%
#                                 group_by(AAA) %>% summarise_at("PK_countryID",list)

# # top20peopleOver70
# CONST_NAME_OF_PRODUCT <-2
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- ''
# CONST_VECTOR_NAME_OF_PRODUCT <- df_Country_DICT %>%
#                                 select (PK_countryID,population2018_A_70) %>%
#                                 arrange(desc(population2018_A_70)) %>% head(20) %>%
#                                 select (PK_countryID)  %>% mutate(AAA="top20peopleOver70")  %>%
#                                 group_by(AAA) %>% summarise_at("PK_countryID",list)

# # top20GPD
# CONST_NAME_OF_PRODUCT <-2
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- ''
# CONST_VECTOR_NAME_OF_PRODUCT <- df_Country_DICT %>%
#                                 select (PK_countryID,GDP_US) %>%
#                                 arrange(desc(GDP_US)) %>% head(20) %>%
#                                 select (PK_countryID)  %>% mutate(AAA="top20GPD")  %>%
#                                 group_by(AAA) %>% summarise_at("PK_countryID",list)

# # top20peoplePerM2
# CONST_NAME_OF_PRODUCT <-2
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- ''
# CONST_VECTOR_NAME_OF_PRODUCT <- df_Country_DICT %>%
#                                 select (PK_countryID,population2018_A,area) %>% mutate(populationPerM2 = population2018_A/area) %>%
#                                 arrange(desc(populationPerM2)) %>% head(20) %>%
#                                 select (PK_countryID)  %>% mutate(AAA="top20peoplePerM2")  %>%
#                                 group_by(AAA) %>% summarise_at("PK_countryID",list)



#######################################################
#
# MAIN - EXTRACT (IMPORT) DATA SECTION
#
#######################################################

#Use data with new dataSet for heatmap
data_table_00 <- df_COVID19Base
data_table_01 <- data_table_00 
                     
# wybierz rodzaj grupowania/wybierania danych do tabeli
if (CONST_NAME_OF_PRODUCT == 1) {
  #grupwanie panstwami   
    v_filerRightSite <- df_Country_DICT$PK_countryID
  } else { 
    v_filerRightSite <- as.vector(unlist(CONST_VECTOR_NAME_OF_PRODUCT[, 1]))
  }

#create html tabel in loop procedure
for ( x_filterPosition in v_filerRightSite){

# wybierz rodzaj grupowanie/wybierania danych do tabeli
if (CONST_NAME_OF_PRODUCT == 1) {
    filterCountry <- as.vector(unlist(df_Country_DICT[df_Country_DICT$PK_countryID==x_filterPosition, m_FieldFilterValue])) 
    filterCountry <- na.omit(c(filterCountry, x_filterPosition))
} else {
  filterCountry <- as.vector(unlist(CONST_VECTOR_NAME_OF_PRODUCT[CONST_VECTOR_NAME_OF_PRODUCT[,1]==x_filterPosition,2]))
}

  #add title
if ( CONST_NAME_OF_PRODUCT_SUFIX =='_country') {
    v_table_title <- paste0('TABLE:',lookupDict(df_Country_DICT, "PK_countryID", x_filterPosition , "countryCommonName"),'. This is Info about COVID-19.')
} else {
  if (CONST_NAME_OF_PRODUCT == 1) {
    v_table_title <- paste0('TABLE countres from set of: ', CONST_NAME_OF_PRODUCT_SUFIX,' for country: ',lookupDict(df_Country_DICT, "PK_countryID", x_filterPosition , "countryCommonName"),'. This is Info about COVID-19.')
  } else {
    v_table_title <- paste0('TABLE countres from set of: ', x_filterPosition ,': This is Info about COVID-19.')
    
  }
}    
  
#filtering data for table
data_table_02 <- data_table_01[which(data_table_01$PK_countryID %in% filterCountry),]

#create final data set or parameter for table
if (CONST_TABLE_TYP == 1){
  data_table_03 <-  data_table_02[data_table_02$PK_dateReport1DayNatural == as.Date('10/06/2020', "%d/%m/%Y"),f_orderColumnFinallDataSet("v_normal_table_base")]
  data_table_03 <- mutate(data_table_03, date1Day1cumSumDeathP1mAll = PK_dateReport1DayNatural - index1Day1cumSumDeathP1mAll )
  data_table_03 <- mutate(data_table_03, cumSumDeath1DayNaturalP1mAll = round((cumSumDeath1DayNatural/population2018_A)*10^6))
  data_table_03 <- mutate(data_table_03, cumSumDeath1DayNaturalP100t_A_70 =  round((cumSumDeath1DayNatural/population2018_A_70)*10^5))
  data_table_03 <-  data_table_03[,f_orderColumnFinallDataSet("v_normal_table_result")]
  v_table_columns <- f_orderColumnFinallDataSet("v_normal_table_columns")
  v_table_width <- '780px'
  
}  else if (CONST_TABLE_TYP == 2){
  data_table_03 <-  data_table_02[,f_orderColumnFinallDataSet("v_big_table_result")]
  v_table_columns <- f_orderColumnFinallDataSet("v_big_table_columns")
  v_table_width <- '1380px'
}

#generate html table
tabGeneralHtml <- datatable(data_table_03, 
                            extensions = c('Buttons', 'ColReorder', 'KeyTable'),
                            options = list(
                              autoWidth = TRUE,
                              pageLength = 15,
                              searching = FALSE,
                              dom = 'Bfrtip',
                              buttons = c('copy', 'csv', 'excel'),
                              colReorder = TRUE, 
                              keys = TRUE,
                              deferRender = TRUE
                            ),
                            width=v_table_width,
                            colnames = v_table_columns,
                            rownames = FALSE,
                            caption = v_table_title
)

#save html table
saveWidget(tabGeneralHtml, paste0(CONST_EXPORT_PATH,CONST_NAME_OF_PRODUCT_TYP[CONST_TABLE_TYP] ,'_',CONST_NAME_OF_PRODUCT_PREFIX,x_filterPosition,CONST_NAME_OF_PRODUCT_SUFIX,'.html'))
}


#######################################################
#
# MAIN - LOAD (EXPORT) DATA SECTION
#
#######################################################
# export data to databas or other packages

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


