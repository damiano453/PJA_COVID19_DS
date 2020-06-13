#R project       
#Script name     :covid19_01MAP_v10.R
#Description     :This script will create map about covic19 
#
#
#Author		               :Roman Ch¹dzyñski
#Last modification date  :2020-04-19
#Last R_version    :R version 3.6.3
#
# v1.0 by Roman Ch¹dzyñski - initial version

#TO DO
#1. legenda
#2 funcja
#######################################################
#
# INIT - INITIAL SECTION
#
#######################################################

#clear terminal
cat("\f")

#Remove all variables without CONST_IMPORT_DATASET
CONST_IMPORT_DATASET = c('df_COVID19Base', 'df_Country_DICT')
ls_VariablesToRm <- NULL
ls_VariablesToRm <- ls()
#jezeli nie ma to od razu b³ad i wyjscie
ls_VariablesToRm <-ls_VariablesToRm[! ls_VariablesToRm %in% CONST_IMPORT_DATASET]
rm(list=ls_VariablesToRm)

#clear memory
gc() 

###############################
# INIT- LIBRARY SECTION

#these libraries are necessary
if(!require("utils")) install.packages("utils")
library(utils)
if(!require("leaflet")) install.packages("leaflet")
library(leaflet)
if(!require("rgdal")) install.packages("rgdal")
library(rgdal)
if(!require("lubridate")) install.packages("lubridate")
library(lubridate)
if(!require("htmltools")) install.packages("htmltools")
library(htmltools)
if(!require("zoo")) install.packages("zoo")
library(zoo)
if(!require("mapview")) install.packages("mapview")
library(mapview)
if(!require("webshot")) install.packages("webshot")
library(webshot)
if(!require("htmlwidgets")) install.packages("htmlwidgets")
library(htmlwidgets)
install_phantomjs()

###############################
# INIT- FUNCTION AND SECTION

#function add title of map
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 16px;
  }
"))

#function error handling
trySilent <- function(code, silent =TRUE) {
  tryCatch(code, error = function(c) {
    msg <- conditionMessage(c)
    if (!silent) message(c)
    invisible(structure(msg, class = "try-error"))
  })
}

#create geometric seqences - form range collor palette
geomSeq <- function(start,ratio,begin,end){
  begin=begin-1
  end=end-1
  start*ratio**(begin:end)
}


###############################
# INIT- CONST AND GLOBAL VAR SECTION

#Exported or dont clear datasets
CONST_EXPORT_DATASET <- CONST_IMPORT_DATASET

#use data with new dataSet for Covid
df_COVID19Base_MAP <- df_COVID19Base

#files with country shape (map) 
CONST_COUTRYSHAPE_FILE <- "C:/R_project_pja/shape/Countries_WGS84"

#path to save map file
CONST_save_folder_name <- "C:/R_project_pja/map/"

#key for identity country 
CONST_COUNTRY_KEY <- "country"
CONST_COUNTRYID_KEY <- "countryID"

#date for first and last (map) 
CONST_START_DATE_MAP <- '2020-04-24'
CONST_STOP_DATE_MAP <- ymd((today("GMT") -1)) # default yesterday

#CONSTANT part of map
#copyright
CONST_strCopyrightHtml <- "MIT License, Copyright (c) [2020] [Roman Chadzymski]"
CONST_copyrightHtml <- tags$div(HTML(CONST_strCopyrightHtml))  

CONST_strCopyrightPng <- "<font size='1'>MIT License,(c) [Roman Chadznski]</font>"
CONST_copyrightPng <- tags$div(HTML(CONST_strCopyrightPng))  

#add a color palette for cumulative deaths - "CumDeath"
colors_CumDeath <-  c('green','#238b45','#a1d99b','#fec69d','#fd9c55', '#fd8d3c', '#fc4e2a', '#e31a1c', '#e31a1c', '#800026', 'black')
value_gt_CumDeath <- c(-1,3,6,12,24,48,96,2200, 6500,13000, 26000)
value_lo_CumDeath <- c(10,20,40,80,250,750,2200,6500,13000,26000,100000)

#add a color palette for cumulative deaths - "DeathPre1m70"
colors_CumDeathPre1m70 <- c('green','#238b45','#a1d99b','#fec69d','#fd9c55', '#fd8d3c', '#fc4e2a', '#e31a1c', '#e31a1c', '#800026', 'black')
value_gt_CumDeathPre1m70 <- c(-1,geomSeq(4, 2 , 1, 10))
value_lo_CumDeathPre1m70 <- c(4,geomSeq(4, 2 , 2, 11))
value_lo_CumDeathPre1m70[length(value_lo_CumDeathPre1m70)] <- value_lo_CumDeathPre1m70[length(value_lo_CumDeathPre1m70)]*2


#add a color palette for cumulative deaths - "DeathPre1mA"
colors_CumDeathPre1mA <- c('green','#238b45','#a1d99b','#fec69d','#fd9c55', '#fd8d3c', '#fc4e2a', '#e31a1c', '#e31a1c', '#800026', 'black')
value_gt_CumDeathPre1mA <- c(-1,geomSeq(1, 2 , 1, 10))
value_lo_CumDeathPre1mA <- c(4,geomSeq(1, 2 , 2, 11))
value_lo_CumDeathPre1mA[length(value_lo_CumDeathPre1mA)] <- value_lo_CumDeathPre1mA[length(value_lo_CumDeathPre1mA)]*2

#add dataFrame color palette 
CONST_df_collor_palette <- data.frame(colors_CumDeath, value_gt_CumDeath, value_lo_CumDeath,
                                      colors_CumDeathPre1m70, value_gt_CumDeathPre1m70, value_lo_CumDeathPre1m70, 
                                      colors_CumDeathPre1mA, value_gt_CumDeathPre1mA, value_lo_CumDeathPre1mA) 
#removes vector use to create dataFrame
rm( list= c("colors_CumDeath", "value_gt_CumDeath", "value_lo_CumDeath", 
            "colors_CumDeathPre1m70", "value_gt_CumDeathPre1m70", "value_lo_CumDeathPre1m70", 
            "colors_CumDeathPre1mA", "value_gt_CumDeathPre1mA", "value_lo_CumDeathPre1mA")) 

#add title form map
CONST_prefix_file_for_use <- c(1,2,3)
CONST_prefix_file  <- c("CumDeath","CumDeathPre1m70", "CumDeathPre1mA")
CONST_title_map <- c("summary mortality -- MAP OF COVID-19 CORONAVIRUS PANDEMIC" ,"MAP OF COVID-19 CORONAVIRUS PANDEMIC (mortality per capita over 70)","mortality per capita -- MAP OF COVID-19 CORONAVIRUS PANDEMIC")

#######################################################
#
# MAIN - EXTRACT (IMPORT) DATA SECTION
#
#######################################################


###############################
# MAIN - Load shape of map 

#load map layer with shape 
Country.map <- readOGR(dsn = CONST_COUTRYSHAPE_FILE , layer = "Countries_WGS84", stringsAsFactors = FALSE)
setnames(Country.map@data, "CNTRY_NAME", CONST_COUNTRY_KEY)

#join (enrichment) add shapes
df_country_OK <- merge(x=Country.map@data, y=df_Country_DICT, by = CONST_COUNTRY_KEY, all.x=TRUE)  
setnames(df_country_OK, "countryID",  "countryID_01")
df_country_OK <- merge(x=df_country_OK, y=df_Country_DICT, by.x = CONST_COUNTRY_KEY, by.y = 'countryCommonName' , all.x=TRUE) 
setnames(df_country_OK, "countryID",  "countryID_02")
df_country_OK <- merge(x=df_country_OK, y=df_Country_DICT, by.x = CONST_COUNTRY_KEY, by.y = 'countryOfficialName' , all.x=TRUE) 
setnames(df_country_OK, "countryID",  "countryID_03")
df_country_OK <- df_country_OK[, c("country", 'OBJECTID', 'countryID_01','countryID_02','countryID_03')]
df_country_OK <- unite(df_country_OK, 'countryID' , c('countryID_01','countryID_02','countryID_03'))
df_country_OK$countryID <- gsub('NA_', '', df_country_OK$countryID)
df_country_OK$countryID <- substr(df_country_OK$countryID,1,3)

#coutryID exeption
df_country_OK[df_country_OK$OBJECTID==21,]$countryID <- 'BHS' #Bahamas BHS
df_country_OK[df_country_OK$OBJECTID==26,]$countryID <- 'MMR' #Myanma MMR
df_country_OK[df_country_OK$OBJECTID==28,]$countryID <- 'BLR' #Belarus BLR
df_country_OK[df_country_OK$OBJECTID==80,]$countryID <- 'GMB' #Gambia  GMB

#coutryID has to by not null 
df_country_OKF <- df_country_OK[df_country_OK$countryID!='NA',]
#coutryID has to by null, ERROR SET
df_country_NA <- df_country_OK[df_country_OK$countryID=='NA',]

#join (enrichment) add shapes to coutryID
Country.map <- merge(x=Country.map, y=df_country_OKF[,c('OBJECTID',"countryID")], by = 'OBJECTID', all=FALSE) 

###############################
# MAIN - add colums (enrichment) with color pallet to base co  

#add a color palette based on the cumulative number of deaths
paletteColor <- CONST_df_collor_palette[,c(1:3)]
df_COVID19Base_MAP[,'colors_CumDeath'] <- "green"
for (i in 1:11) { 
  trySilent(df_COVID19Base_MAP[(!is.na(df_COVID19Base_MAP$cumsumDeaths)) & df_COVID19Base_MAP$cumsumDeaths>paletteColor[i,2] & df_COVID19Base_MAP$cumsumDeaths<=paletteColor[i,3],'colors_CumDeath'] <- as.character(paletteColor[i,1]))

  }
#add a color palette based on the cumulative number of deaths per DeathsPer1m7
paletteColor <- CONST_df_collor_palette[,c(4:6)]
df_COVID19Base_MAP[,'colors_CumDeathPre1m70'] <- "green"
for (i in 1:11) { 
  trySilent(df_COVID19Base_MAP[(!is.na(df_COVID19Base_MAP$cumsumDeathsPer1m70)) & df_COVID19Base_MAP$cumsumDeathsPer1m70>paletteColor[i,2] & df_COVID19Base_MAP$cumsumDeathsPer1m70<=paletteColor[i,3],'colors_CumDeathPre1m70'] <- as.character(paletteColor[i,1]))
  }

#add a color palette based on the cumulative number of deaths per DeathsPer1mA
paletteColor <- CONST_df_collor_palette[,c(7:9)]
df_COVID19Base_MAP[,'colors_CumDeathPre1mA'] <- "green"
for (i in 1:11) { 
  trySilent(df_COVID19Base_MAP[(!is.na(df_COVID19Base_MAP$cumsumDeathsPer1mA)) & df_COVID19Base_MAP$cumsumDeathsPer1mA>paletteColor[i,2] & df_COVID19Base_MAP$cumsumDeathsPer1mA<=paletteColor[i,3],]$colors_CumDeathPre1mA <- as.character(paletteColor[i,1]))
}


###############################
# MAIN - Genreate of map 

for (dateRaport in  seq(from = as.Date(CONST_START_DATE_MAP), to = as.Date(CONST_STOP_DATE_MAP), by = 1)) {
print(format(as.Date(dateRaport), "%d/%m/%Y"))


#filter data for one map (on date) 
data_M_01 <- df_COVID19Base_MAP[df_COVID19Base_MAP$dateRep==format(as.Date(dateRaport), "%d/%m/%Y"),]
data_M_01$ID <- seq.int(nrow(data_M_01))

#join (enrichment) add shapes
data_M_01 <- merge(x=Country.map, y=data_M_01, by = CONST_COUNTRYID_KEY, all=FALSE)  

#crete label for map 
PopulationOver70 = data_M_01@data$popData2018_F_70_74 + data_M_01@data$popData2018_M_70_74 + data_M_01@data$popData2018_F_75_79 + data_M_01@data$popData2018_M_75_79+ data_M_01@data$popData2018_F_80 + data_M_01@data$popData2018_M_80
data_M_01@data$label <- paste("<p><font size='3'> <b>",data_M_01@data$countryOfficialName,"</b></font></p>",
                        "<p>deaths:",data_M_01@data$cumsumDeaths ,"</p>",
                        "<p>daysAfter10Deaths:",data_M_01@data$index10Deaths ,"</p>",
                        "<p>weeksAfter10Deaths:",data_M_01@data$index10WeekDeaths,"</p>",
                        "<p>-----------------------------------------</p>",
                        "<p>cases:",data_M_01@data$cumsumCases,"</p>",
                        "<p>daysAfter100Cases:",data_M_01@data$index100Cases,"</p>",
                        "<p>weeksAfter100Cases:",data_M_01@data$index100WeekCases,"</p>",
                        "<p>-----------------------------------------</p>",
                        "<p>population:",ceiling(data_M_01@data$popData2018/(10^6)),"mln</p>",
                        "<p>populationOver70:",ceiling(PopulationOver70/(10^6)),"mln</p>"
)



# dg data_M_01 , vector colorki, tytu³ 
generateMap <- function(colorsShape, titleMap, save_prefix_name) {
map_html <-leaflet(data = data_M_01) %>%
  addTiles() %>%
  addControl(titleMap, position = "topleft", className="map-title") %>%
  addControl(CONST_copyrightHtml, position = "bottomleft") %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  setView(lat = 52, lng=20, zoom = 3)  %>%
  addPolygons(fillColor =  colorsShape,
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1) %>%
  addCircleMarkers(lng = data_M_01@data$lng, 
                   lat = data_M_01@data$lat,
                   color = "white",
                   radius = 0,
                   label = lapply(data_M_01@data$label,HTML),
                   group = 'general')%>%
  addCircleMarkers(lng = data_M_01@data$lng, 
                   lat = data_M_01@data$lat,
                   color = '#1919ff',
                   radius = ifelse(data_M_01@data$index10WeekDeaths<3, data_M_01@data$index10WeekDeaths*5,0),
                   label = lapply(data_M_01@data$label,HTML),
                   group = '1-2_weeksAfter10Death') %>%
  addCircleMarkers(lng = data_M_01@data$lng, 
                   lat = data_M_01@data$lat,
                   color = '#00004c',
                   radius = ifelse((data_M_01@data$index10WeekDeaths>2 & data_M_01@data$index10WeekDeaths<5), data_M_01@data$index10WeekDeaths*5,0),
                   label = lapply(data_M_01@data$label,HTML),
                   group = '3-4_weeksAfter10Death') %>%
  addCircleMarkers(lng = data_M_01@data$lng, 
                   lat = data_M_01@data$lat,
                   color = '#000000',
                   radius = ifelse((data_M_01@data$index10WeekDeaths>4 & data_M_01@data$index10WeekDeaths<7), data_M_01@data$index10WeekDeaths*5,0),
                   label = lapply(data_M_01@data$label,HTML),
                   group = '5-6_weeksAfter10Death') %>%
  addCircleMarkers(lng = data_M_01@data$lng, 
                   lat = data_M_01@data$lat,
                   color = '#b2b2ff',
                   radius = ifelse((data_M_01@data$index10WeekDeaths>6), data_M_01@data$index10WeekDeaths*5,0),
                   label = lapply(data_M_01@data$label,HTML),
                   group = '>=7_weeksAfter10Death') %>%
  addCircleMarkers(lng = data_M_01@data$lng, 
                   lat = data_M_01@data$lat,
                   color = 'red',
                   radius = ifelse(((data_M_01@data$avgDeaths10DWeek-data_M_01@data$avgDeaths10DWeekPrv)/data_M_01@data$avgDeaths10DWeek)<0,0,10),
                   label = lapply(data_M_01@data$label,HTML),
                   group = 'avgDeath-UP') %>%
  addCircleMarkers(lng = data_M_01@data$lng, 
                   lat = data_M_01@data$lat,
                   color = 'green',
                   radius = ifelse(((data_M_01@data$avgDeaths10DWeek-data_M_01@data$avgDeaths10DWeekPrv)/data_M_01@data$avgDeaths10DWeek)<0,10,0),
                   label = lapply(data_M_01@data$label,HTML),
                   group = 'avgDeath-DOWN') %>%
  addCircleMarkers(lng = data_M_01@data$lng, 
                   lat = data_M_01@data$lat,
                   color = 'green',
                   radius = ifelse(((data_M_01@data$avgDeaths10DWeek-data_M_01@data$avgDeaths10DWeekPrv)/data_M_01@data$avgDeaths10DWeek)<0,10,0),
                   label = lapply(data_M_01@data$label,HTML),
                   group = 'avgDeath-DOWN') %>%
  addCircleMarkers(lng = data_M_01@data$lng, 
                   lat = data_M_01@data$lat,
                   color = 'yellow',
                   radius = ((data_M_01@data$popData2018_F_70_74 + data_M_01@data$popData2018_M_70_74 + data_M_01@data$popData2018_F_75_79 + data_M_01@data$popData2018_M_75_79+ data_M_01@data$popData2018_F_80 + data_M_01@data$popData2018_M_80)/(10^6)),
                   label = lapply(data_M_01@data$label,HTML),
                   group = 'popultionOver70mln') %>%
  addLayersControl(baseGroups = c("(default)","1-2_weeksAfter10Death","3-4_weeksAfter10Death","5-6_weeksAfter10Death",">=7_weeksAfter10Death", "popultionOver70mln", "avgDeath-UP", "avgDeath-DOWN"),
                   overlayGroups = c("avgDeath-UP", "avgDeath-DOWN","popultionOver70mln"),
                   options = layersControlOptions(collapsed = FALSE,  autoZIndex = TRUE)
  )

#save map as file
saveWidget(map_html, file = paste(CONST_save_folder_name,paste(save_prefix_name,paste(as.Date(dateRaport),"html", sep = "."), sep = "_")))
}

for ( x in CONST_prefix_file_for_use) {
  
  #title form map
  strTitle <- paste(CONST_title_map[x], as.Date(dateRaport), sep = ' -- ')
  titleHtml <- tags$div(
    tag.map.title, HTML(strTitle)
  ) 
  
  #prefix map name
  save_prefix_name <- CONST_prefix_file[x]
  
  #color of shape
  name_colour_column <- paste('colors_',CONST_prefix_file[x], sep = '')
  dataColorColumn <- data_M_01@data[,name_colour_column]
  
  #generate map
  generateMap(dataColorColumn, titleHtml, save_prefix_name)
}

}

#######################################################
#
# FINAL SECTION
#
#######################################################
#Remove all variables without CONST_EXPORT_DATASET
ls_VariablesToRm <- NULL
ls_VariablesToRm <- ls()
ls_VariablesToRm <-ls_VariablesToRm[! ls_VariablesToRm %in% CONST_EXPORT_DATASET]
rm(list=ls_VariablesToRm)
gc()