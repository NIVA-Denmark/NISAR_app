library(shiny)
library(leaflet)
library(googlesheets)
library(RColorBrewer)
library(tidyverse)
library(scales)
library(rgdal)

# MISSING REGION INFORMATION FOR eDNA


LANG = "DK"
#LANG = "EN"

map_method_cols <- c("#0000FF","#FF0000","#00FF00")

if(LANG=="DK"){
  RegionList<-c("Nordsøen","Kattegat","Limfjorden","Bælthavet","Vestlig Østersø")
  sMethod <- c("Konventionel","eDNA","Begge")
  sLabelMethod <- "Metode"
  sLabelKingdom <- "Rige"
  sLabelSpecies <- "Art"
  sLabelRegion <- "Region"
  sLabelCount <- "Antal"
  sLabelYear <- "År"
  sLabelGroup <- "Gruppering af punkter"
  sAll <- "ALLE"
  sAppTitle <- "Kort over ikke-hjemmehørende arter"
  sLogoFile<-"www/NIVA-Danmark-150.png"
  method_cols <- c("Konventionel"=map_method_cols[1],
                   "eDNA"=map_method_cols[2],
                   "Begge"=map_method_cols[3])
}else{
  RegionList<-c("North Sea","Kattegat","Limfjord","Belt Sea","W. Baltic")
  sMethod <- c("Conventional","eDNA","Both")
  sLabelMethod <- "Method"
  sLabelKingdom <- "Kingdom"
  sLabelSpecies <- "Species"
  sLabelRegion <- "Region"
  sLabelCount <- "Count"
  sLabelYear <- "Year"
  sLabelGroup <- "Grouping points"
  sAll <- "ALL"
  sAppTitle <- "Map of non-indigenous species"
  sLogoFile<-"www/NIVA-Denmark-150.png"
  method_cols <- c("Conventional"=map_method_cols[1],
                   "eDNA"=map_method_cols[2],
                   "Both"=map_method_cols[3])
}



Region<-factor(c(RegionList,"unspecified"),levels=c(RegionList,"unspecified"))
REGIONID <-c(1,2,3,4,5,0)
dfRegion <-data.frame(Region,REGIONID,stringsAsFactors=T)

YearMin <- 1990 # minimum year for bar chart
YearMax <- as.numeric(format(Sys.Date(),"%Y")) # maximum year for bar chart

YearList <- c(YearMin:YearMax)  

dfSpecies <- read.table("data/NISAR_aphia_id.csv",stringsAsFactors=F,header=T,fileEncoding="UTF-8",sep=";")

dfObs <- read.table("data/NISAR_obs.csv",stringsAsFactors=F,header=T,fileEncoding="UTF-8",sep=";")
dfObs <- dfObs %>%
  left_join(select(dfSpecies,AphiaID,ScientificName),by="AphiaID") %>%
  mutate(REGIONID=ifelse(is.na(REGIONID),0,REGIONID)) %>%
  left_join(dfRegion,by="REGIONID") %>%
  #mutate(Lon=ifelse(is.na(Lon),NA,round(Lon,3))) %>%
  #mutate(Lat=ifelse(is.na(Lat),NA,round(Lat,3))) %>%
  mutate(eDNA=ifelse(is.na(eDNA),FALSE,eDNA)) %>%
  mutate(Method=ifelse(eDNA,sMethod[2],sMethod[1])) %>%
  filter(Year>=1990) 

dfNISlist <- distinct(dfSpecies,AphiaID)

dfKingdom <- dfSpecies %>%
  distinct(AphiaID,Kingdom,Phylum)

dfSpecies <- dfSpecies %>%
  distinct(AphiaID,ScientificName)


shpKU <- readOGR("data/KU_fish_WGS84_simp.shp")

df <- dfObs %>%
  filter(is.na(ShapeID)) %>%
  group_by(Lat,Lon,ScientificName,AphiaID,Year,Region,ShapeID) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  group_by(Lat,Lon,ScientificName,AphiaID,Region,ShapeID) %>%
  summarise(From=min(Year,na.rm=T),To=max(Year,na.rm=T),
            YearList=paste(Year,collapse=","),n=sum(n,na.rm=T)) %>%
  mutate(Years=ifelse(From==To,as.character(From),paste0(From,"-",To))) %>%
  ungroup()

df_shp <- dfObs %>%
  filter(!is.na(ShapeID)) %>%
  group_by(ShapeID,ScientificName,AphiaID,Year,Region) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  group_by(ShapeID,ScientificName,AphiaID,Region) %>%
  summarise(From=min(Year,na.rm=T),To=max(Year,na.rm=T),
            YearList=paste(Year,collapse=","),n=sum(n,na.rm=T)) %>%
  mutate(Years=ifelse(From==To,as.character(From),paste0(From,"-",To))) %>%
  ungroup()


dfDNA <- dfObs %>%
  filter(!(is.na(Lat) | is.na(Lon))) %>%
  mutate(Method=ifelse(eDNA,"eDNA","Conv")) %>%
  group_by(Lat,Lon,AphiaID,Method) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  pivot_wider(values_from = n,names_from = Method) %>%
  mutate(Method=ifelse(is.na(eDNA),sMethod[1],ifelse(is.na(Conv),sMethod[2],sMethod[3])))  %>%
  select(Lat,Lon,AphiaID,Method)


df <- dfNISlist %>%
  left_join(df,by="AphiaID") %>%
  filter(!(is.na(Lat) | is.na(Lon)))

df <- df %>% 
  left_join(dfKingdom,by="AphiaID")

df <- df %>% 
  left_join(dfDNA,by=c("AphiaID","Lat","Lon"))

df <- df %>% 
  mutate(HtmlText=paste0("<a href='http://marinespecies.org/aphia.php?p=taxdetails&id=",AphiaID,
                         "'>http://marinespecies.org/aphia.php?p=taxdetails&id=",AphiaID,"</a>")) %>%
  arrange(ScientificName)

name_list<-c("ALL",sort(unique(dfObs$ScientificName)))

Kingdomlist<- distinct(dfKingdom,Kingdom) %>%
  arrange(Kingdom)
Kingdomlist<-Kingdomlist$Kingdom
