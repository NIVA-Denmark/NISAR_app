library(shiny)
library(leaflet)
library(googlesheets)
library(RColorBrewer)
library(tidyverse)
library(scales)

shinyServer(function(input, output,session) {

  LANG = "DK"
  #LANG = "EN"
  
  if(LANG=="DK"){
    RegionList<-c("Nordsøen","Kattegat","Limfjorden","Bælthavet","Vestlig Østersø")
    sMethod <- c("Konventionel","eDNA","Begge")
    sLabelMethod <- "Metode"
    sLabelKingdom <- "Rige"
    sLabelSpecies <- "Art"
    sLabelRegion <- "Region"
    sLabelCount <- "Antal"
    sLabelYear <- "År"
    sAll <- "ALLE"
    sAppTitle <- "Kort over ikke-hjemmehørende arter"
  }else{
    RegionList<-c("North Sea","Kattegat","Limfjord","Belt Sea","W. Baltic")
    sMethod <- c("Conventional","eDNA","Both")
    sLabelMethod <- "Method"
    sLabelKingdom <- "Kingdom"
    sLabelSpecies <- "Species"
    sLabelRegion <- "Region"
    sLabelCount <- "Count"
    sLabelYear <- "Year"
    sAll <- "ALL"
    sAppTitle <- "Map of non-indigenous species"
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
    mutate(eDNA=ifelse(is.na(eDNA),FALSE,eDNA)) %>%
    mutate(Method=ifelse(eDNA,sMethod[2],sMethod[1])) %>%
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
    left_join(dfSpecies,by="AphiaID")
  
  df <- df %>% 
    left_join(dfDNA,by=c("AphiaID","Lat","Lon"))
  
  df <- df %>% 
    mutate(HtmlText=paste0("<a href='http://marinespecies.org/aphia.php?p=taxdetails&id=",AphiaID,
                           "'>http://marinespecies.org/aphia.php?p=taxdetails&id=",AphiaID,"</a>")) %>%
    arrange(ScientificName)
  
  
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
      if(input$Region!=sAll){
        df1<-df1[df1$Region == input$Region,]
      }
    }
    if(!is.null(input$Method)){
      if(input$Method!=sAll){
        df1<-df1[df1$Method == input$Method,]
      }
    }
    
    df1
    })
  
  MethodList <- reactive({
    df1 <- distinct(df,Method)
    return(df1$Method)
  })

  SpeciesList <- reactive({
    df1<-df
    if(!is.null(input$Kingdom)){
      if(!input$Kingdom==sAll){
        df1<-df1[df1$Kingdom == input$Kingdom,]
      }
    }
    if(!is.null(input$Method)){
      if(!input$Method==sAll){
        df1<-df1[df1$Method == input$Method,]
      }
    }
    df1 <- distinct(df1,ScientificName)
    return(df1$ScientificName)
    })
  
  
  
  output$mymap <- renderLeaflet({
    
    mapdf<-df_r()
    
    method_cols <- c("#999999","#FF0000","#0000FF")

    mapdf <- mapdf %>%
      filter(!is.na(Lat))
    
    pal<-colorFactor(method_cols, mapdf$Method,levels=sMethod)

        map<- leaflet({mapdf}) %>% 
      addProviderTiles("Esri.WorldStreetMap", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(zoom=6,lat=56.5,lng=11.5)
    #browser()
    if(nrow(mapdf)>0){
      map <- map %>% addCircleMarkers(~Lon, ~Lat, radius=5,popup=~YearList,label=~Years,
                       stroke=TRUE, color=~pal(Method),weight=1,opacity=1,fillOpacity = 1,fillColor=~pal(Method))
        
    }

    map
    
  })  
  output$SelectSpecies <- renderUI({
    tagList(
      selectInput("Species",  sLabelSpecies, choices=SpeciesList())
    )})

  output$SelectRegion <- renderUI({
    tagList(
      selectInput("Region", sLabelRegion, choices=c(sAll,RegionList))
    )})
    
  output$SelectKingdom <- renderUI({
    tagList(
      selectInput("Kingdom", sLabelKingdom, choices=Kingdomlist(),multiple=F) #,width='400px'

    )})
  
  output$SelectMethod <- renderUI({
    tagList(
      selectInput("Method", sLabelMethod, choices=c(sAll,MethodList()))
    )})
  
  
  output$AppTitle <- renderText(sAppTitle)
  
  output$barPlot <- renderPlot({

    req(input$Species,input$Method)
    
    dfplot<-dfObs %>%
      filter(ScientificName==input$Species) %>%
      select(ScientificName,Year,Lat,Lon,Region,Method)

    sTitle <- input$Species
    sSubTitle <- ""
    if(!is.null(input$Region)){
      if(input$Region!=sAll){
        dfplot<-dfplot[dfplot$Region == input$Region,]
        sTitle <- paste0(sTitle," ",input$Region,"")
        #sSubTitle <- paste0(sLabelRegion,": ",input$Region)
      }
    }
    if(!is.null(input$Method)){
      if(input$Method!=sAll){
        dfplot<-dfplot[dfplot$Method == input$Method,]
        #sTitle <- paste0(sTitle," (",input$Method,")")
        if(sSubTitle==""){
          sSubTitle <- paste0(sLabelMethod,": ",input$Method)
        }else{
          sSubTitle <- paste0(sSubTitle,", ",sLabelMethod,": ",input$Method)
        }
      }
    }
    if(sSubTitle!=""){
      sTitle <- paste0(sTitle," [",sSubTitle,"]")
    }
    
    dfplot <- dfplot %>%
      group_by(Year,Method) %>%
      summarise(Count=n())
    
    sMethodReplace <- sMethod[1]
    if(nrow(distinct(dfplot,Method))==1){
      sMethodReplace <- dfplot[1,"Method"]
    }
    
    dfplot <- data.frame(Year=YearList) %>%
      left_join(dfplot,by="Year") %>%
      mutate(Count=ifelse(is.na(Count),0,Count)) %>%
      mutate(Method = ifelse(is.na(Method),sMethodReplace,Method)) 
    #dfplot$Count <- as.integer(dfplot$Count)
    
    ymax <- max(dfplot$Count,na.rm=T)
    ymax <- max(ymax,4)
    method_cols <- c("#999999","#FF0000")
    method_cols <- c("Konventionel"="#999999","eDNA"="#FF0000","Begge"="#0000FF")
    
    
    p <- ggplot(dfplot) + geom_bar(stat="identity",position="stack",
                                   aes(x=Year,y=Count,fill=factor(Method,levels=sMethod)),width=0.8) + 
      theme_minimal() +  scale_fill_manual(values = method_cols,name=sLabelMethod) +
      ggtitle(sTitle) + labs(y = sLabelCount, x=sLabelYear) +
      #,subtitle=sSubTitle
      theme(legend.position = "bottom") +
      #scale_y_continuous(labels=comma_format(accuracy=0.1) ) +
      scale_y_continuous(breaks= pretty_breaks()) +
      coord_cartesian(ylim = c(0, ymax))
    
    p
    
  })
  
})
