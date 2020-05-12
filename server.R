library(shiny)
library(leaflet)
library(googlesheets)
library(RColorBrewer)
library(tidyverse)

shinyServer(function(input, output,session) {

  RegionList<-c("North Sea","Kattegat","Limfjord","Belt Sea","W. Baltic")
  Region<-factor(RegionList,levels=RegionList)
  REGIONID <-c(1,2,3,4,5)
  dfRegion <-data.frame(Region,REGIONID,stringsAsFactors=T)
  
  YearMin <- 1990 # minimum year for bar chart
  YearMax <- as.numeric(format(Sys.Date(),"%Y")) # maximum year for bar chart
  
  YearList <- c(YearMin:YearMax)  
  
  dfSpecies <- read.table("data/NISAR_aphia_id.csv",stringsAsFactors=F,header=T,fileEncoding="UTF-8",sep=";")
  dfObs <- read.table("data/NISAR_obs.csv",stringsAsFactors=F,header=T,fileEncoding="UTF-8",sep=";")
  dfObs <- dfObs %>%
    left_join(select(dfSpecies,AphiaID,ScientificName),by="AphiaID") %>%
    #mutate(REGIONID=ifelse(is.na(REGIONID),0,REGIONID)) %>%
    left_join(dfRegion,by="REGIONID") %>%
    filter(Year>=1990) 
  
  dfNISlist <- distinct(dfSpecies,AphiaID)
  
  dfSpecies <- dfSpecies %>%
    distinct(AphiaID,Kingdom,Phylum)
  
  df <- dfObs %>%
    group_by(Lat,Lon,ScientificName,AphiaID,Year,Region) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    group_by(Lat,Lon,ScientificName,AphiaID,Region) %>%
    summarise(From=min(Year,na.rm=T),To=max(Year,na.rm=T),
              YearList=paste(Year,collapse=","),n=sum(n,na.rm=T)) %>%
    mutate(Years=ifelse(From==To,as.character(From),paste0(From,"-",To))) %>%
    ungroup()
  
  df <- dfNISlist %>%
    left_join(df,by="AphiaID") %>%
    filter(!(is.na(Lat) | is.na(Lon)))
  
  df <- df %>% 
    left_join(dfSpecies,by="AphiaID")
  
  
  df <- df %>% 
    mutate(HtmlText=paste0("<a href='http://marinespecies.org/aphia.php?p=taxdetails&id=",AphiaID,
                           "'>http://marinespecies.org/aphia.php?p=taxdetails&id=",AphiaID,"</a>")) %>%
    arrange(ScientificName)
  
  dfYear <- dfObs %>%
    select(ScientificName,Year,Lat,Lon,Region) %>%
    arrange(ScientificName,Year)
  
  name_list<-reactive({
    c("ALL",sort(unique(df$ScientificName)))
  })
  
  Kingdomlist<- reactive({
     df1 <- distinct(dfSpecies,Kingdom)
     return(df1$Kingdom)
  })
  
  df_r<-reactive({
    df1 <- df[0,]
    if(!is.null(input$Species)){
         df1<-df[df$ScientificName == input$Species,]
    }
    if(!is.null(input$Region)){
      if(input$Region!="ALL"){
        df1<-df1[df1$Region == input$Region,]
      }
    }
    
    df1
    })
  
  
  
  SpeciesList <- reactive({
    df1<-df
    if(!is.null(input$Kingdom)){
      if(!input$Kingdom=="ALL"){
        df1<-df1[df1$Kingdom == input$Kingdom,]
      }
    }
    df1 <- distinct(df1,ScientificName)
    return(df1$ScientificName)
    })
  
  strPalName<-brewer.pal(3,"Set1")
  
  output$mymap <- renderLeaflet({
    mapdf<-df_r()
    
    mapdf <- mapdf %>%
      filter(!is.na(Lat))
    pal<-colorFactor(strPalName, df$Kingdom)

        map<- leaflet({mapdf}) %>% 
      addProviderTiles("Esri.WorldStreetMap", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(zoom=6,lat=56.5,lng=11.5)
    
    if(nrow(mapdf)>0){
      map <- map %>% addCircleMarkers(~Lon, ~Lat, radius=5,popup=~YearList,label=~Years,
                       stroke=TRUE, color=~From,weight=1,opacity=1,fillOpacity = 1,fillColor=~From)
        
    }

    map
    
  })  
  output$SelectSpecies <- renderUI({
    tagList(
      selectInput("Species", "Vælg art:", choices=SpeciesList())
    )})

  output$SelectRegion <- renderUI({
    tagList(
      selectInput("Region", "Region:", choices=c("ALL",RegionList))
    )})
    
  output$SelectKingdom <- renderUI({
    tagList(
      selectInput("Kingdom", "Kingdom:", choices=Kingdomlist(),multiple=F) #,width='400px'

    )})
  
  output$barPlot <- renderPlot({
    dfplot<-dfYear %>%
      filter(ScientificName==input$Species)
    
    sTitle <- input$Species
    if(!is.null(input$Region)){
      if(input$Region!="ALL"){
        dfplot<-dfplot[dfplot$Region == input$Region,]
        sTitle <- paste0(sTitle," (",input$Region,")")
      }
    }
    
    dfplot <- dfplot %>%
      group_by(Year) %>%
      summarise(Count=n())
    
    dfplot <- data.frame(Year=YearList) %>%
      left_join(dfplot,by="Year") %>%
      mutate(Count=ifelse(is.na(Count),0,Count))
    
    
    p <- ggplot(dfplot) + geom_bar(stat="identity",position="stack",
                                   aes(x=Year,y=Count),fill="#999999",width=0.8) + 
      theme_minimal() +
      ggtitle(sTitle) 
    
    p
    
  })
  
  
})
