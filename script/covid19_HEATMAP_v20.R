#R project       
#Script name     :covid19_HEATMAP_v20.R
#Description     :This script will create heat map and dendrogram.
#                 Base dataSet is covid19_Base and df_Country_DICT   
#
#
#Author		               :Roman Ch¹dzyñski
#Last modification date  :2020-04-19
#Last R_version          :R version 3.6.3
#
# v1.0 by Roman Ch¹dzyñski - initial version
# v2.0 by Roman Ch¹dzyñski - final version

#TODO:


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

if(!require("heatmaply")) install.packages("heatmaply")
library(heatmaply)

if(!require("RColorBrewer")) install.packages("RColorBrewer")
library(RColorBrewer)

if(!require("pheatmap")) install.packages("pheatmap")
library(pheatmap)

if(!require("dendextend")) install.packages("dendextend")
library(dendextend)

if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("plotly")) install.packages("plotly")
library(plotly)

#biblioteki do sprawdzenia  czy ich uzy³em 
if(!require("hrbrthemes")) install.packages("hrbrthemes")
library(hrbrthemes)
if(!require("viridis")) install.packages("viridis")
library(viridis)


###############################
# INIT- FUNCTION AND SECTION


###############################
# INIT- CONST AND GLOBAL VAR SECTION

#Exported dataset
CONST_EXPORT_DATASET <- CONST_IMPORT_DATASET

#path to export heat map
CONST_EXPORT_PATH <- "C:/R_project_pja/heatmap/"

#Key for identity country 
CONST_COUNTRYID_KEY <- "PK_countryID"

#Which field will map from baseSet t HeaTSEt
m_indexHeatMap <- "index1Day1cumSumDeathP1mAll"
m_PK_countryID <- "PK_countryID"
m_avgHeatMapValue <- "avgDeaths7DayNatural"
m_standarysationField <- "population2018_A"
CONST_STANDARYSATION <- 10^6

#Type of selection
#1. list of selection is from country record (name of product is cod of coutry plus sufix)
# CONST_NAME_OF_PRODUCT <- 1
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_DIST500'
# m_FieldFilterValue <- 'c_groupDistance1000'

# CONST_NAME_OF_PRODUCT <- 1
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_DIST500'
# m_FieldFilterValue <- 'c_groupDistance500'

# CONST_NAME_OF_PRODUCT <- 1
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_DIST500'
# m_FieldFilterValue <- 'c_groupDistance300'

# CONST_NAME_OF_PRODUCT <- 1
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_neighbors'
# m_FieldFilterValue <-  "c_neighbors"

# CONST_NAME_OF_PRODUCT <- 1
# CONST_NAME_OF_PRODUCT_PREFIX <- ''
# CONST_NAME_OF_PRODUCT_SUFIX <- '_region'
# m_FieldFilterValue <- "c_region" 

# CONST_NAME_OF_PRODUCT <- 1
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

#First day of week to filter data to heatmap
CONST_FIRST_DAYOFWEEK <- seq(from=1,to=70,by=7)
CONST_HOW_MANY_WEEK <- 10

#######################################################
#
# MAIN - EXTRACT (IMPORT) DATA SECTION
#
#######################################################

#Use data with new dataSet for heatmap
data_heat_00 <- df_COVID19Base[,c(m_PK_countryID,m_indexHeatMap, m_avgHeatMapValue)]
colnames(data_heat_00) <-c("PK_countryID","indexHeatMap", "avgHeatMapValue")

# Create pivot SET with data to heat map
data_heat_01 <- data_heat_00[data_heat_00$indexHeatMap %in% CONST_FIRST_DAYOFWEEK,]
date_heat_01  <- cast(data_heat_01, PK_countryID ~ indexHeatMap, value="avgHeatMapValue", fun.aggregate =  mean )

#join (enrichment) date fromdf_Country_DICT
date_heat_02 <- merge(x=date_heat_01, y=df_Country_DICT, by = CONST_COUNTRYID_KEY , all.x=TRUE) 

# make a standarisation of data 
date_heat_02$standarysationField <- date_heat_02[,m_standarysationField]/CONST_STANDARYSATION
date_heat_02[,seq(from=2,to=CONST_HOW_MANY_WEEK+1,by=1)] <- round(date_heat_02[,seq(from=2,to=CONST_HOW_MANY_WEEK+1,by=1)]/date_heat_02[,c("standarysationField")],2)

# renamen series of data  
row.names(date_heat_02) <- date_heat_02$countryCommonName

# wybierz rodzaj grupowanie danych do heatmap
if (CONST_NAME_OF_PRODUCT == 1) {
  #grupwanie panstwami   
  v_filerRightSite <- date_heat_02$PK_countryID
  } else { 
    v_filerRightSite <- as.vector(unlist(CONST_VECTOR_NAME_OF_PRODUCT[, 1]))
  }

#create heatmap in loop procedure
for ( x_filterPosition in v_filerRightSite){

  # wybierz rodzaj grupowanie danyc do heatmap
if (CONST_NAME_OF_PRODUCT == 1) {
    filterCountry <- as.vector(unlist(date_heat_02[date_heat_02$PK_countryID==x_filterPosition, m_FieldFilterValue])) 
    filterCountry <- na.omit(c(filterCountry, x_filterPosition))
   
} else {
  filterCountry <- as.vector(unlist(CONST_VECTOR_NAME_OF_PRODUCT[CONST_VECTOR_NAME_OF_PRODUCT[,1]==x_filterPosition,2]))
}
  
#filtering and transform data to matrix ()
mx_heat_01 <- as.matrix(date_heat_02[date_heat_02$PK_countryID %in% filterCountry, c(seq(from=2,to=CONST_HOW_MANY_WEEK+1,by=1))])

#print haetmap for set >2  
if (nrow(mx_heat_01)>2 ){

#cerate matrix with fisical valu (form show in heeat map, heatmap is generate based on standarizated column value)  
mx_heat_02 <- round(mx_heat_01,2)
mx_heat_02[is.na(mx_heat_02)] <- "" 

#create dendrogram
if (nrow(mx_heat_01)> 5) {
  howManyClass <- 5} else
  {howManyClass <- 2}  

row_dend  <- mx_heat_01 %>% 
  dist %>% 
  hclust %>% 
  as.dendrogram %>%
  set("branches_k_color", k = howManyClass) %>%
  set("branches_lwd", c(1)) %>%
  ladderize

#const canvas form creating heatmap and dendrogram 
gg_back_box <-  theme(
  panel.background = element_rect(fill =  "#D4D9DB"),
  plot.background = element_rect(fill =   "#D4D9DB", size = 1),
  legend.background = element_rect(fill = "#D4D9DB"),
  axis.ticks.length = unit(0.001, "mm")
) 

#heatmap is generate based on standarizated column value
mx_heat_NA <-mx_heat_01
mx_heat_NA[is.na(mx_heat_NA)] <- -0.0000001 
mx_heat_NA <- percentize(mx_heat_NA)

#add value standarizated 
for(row in 1:nrow(mx_heat_01)) {
  for(col in 1:ncol(mx_heat_01)) {
    if (!is.na(mx_heat_01[row, col])) {mx_heat_01[row, col] <- mx_heat_NA[row, col]}
  }
}

#generate heatmap
p <- heatmaply(mx_heat_01, 
               color = brewer.pal(9,"Oranges"),    
               dendrogram = "row",
               na.rm = TRUE,
               Rowv = row_dend,
               cellnote = mx_heat_02,
               cellnote_size = 10,
               cellnote_textposition = "middle center",
               xlab = "", ylab = "", 
               main = paste("HeatMap of pandemic COVID-19 \ndeath per capita for group ", x_filterPosition, CONST_NAME_OF_PRODUCT_SUFIX) ,
               #scale = "column",
               margins = c(10,10,50,10),
               grid_color = "white",
               grid_width = 0.00003,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.1,
               label_names = c("Country", "Week:", "Value"),
               fontsize_row = 9, fontsize_col = 14,
               labCol = c(1:CONST_HOW_MANY_WEEK),
               labRow = rownames(mx_heat_01),
               heatmap_layers = gg_back_box,
               file = paste0(CONST_EXPORT_PATH,CONST_NAME_OF_PRODUCT_PREFIX,x_filterPosition,CONST_NAME_OF_PRODUCT_SUFIX,'.html')
)
}

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


