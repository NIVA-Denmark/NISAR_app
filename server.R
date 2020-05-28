

shinyServer(function(input, output,session) {

  df_r<-reactive({
    df0 <- df %>%
      filter(is.na(ShapeID))
    df1 <- df0[0,]
    if(!is.null(input$Species)){
         df1<-df0[df$ScientificName == input$Species,]
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
  
  
  df_shp_r<-reactive({
    df0 <- df %>%
      filter(!is.na(ShapeID))
    df1 <- df0[0,]    
    if(!is.null(input$Species)){
      df1<-df_shp[df_shp$ScientificName == input$Species,]
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
  
  shp_r <- reactive({

    df_shp_select <- df_shp_r()
    idlist <- df_shp_select$ShapeID
    shp <-subset(shpKU, KUID %in% idlist)
    
    df_dat <- shp@data
    
    df_dat <- df_dat %>%
      left_join(df_shp_select,by=c("KUID"="ShapeID"))
    
    shp@data <- df_dat
    shp
  })
  

  
  MethodList <- reactive({
    df1 <- distinct(df,Method)
    return(df1$Method)
  })

  SpeciesList <- reactive({
    #browser()
    
    df1<-dfObs %>% 
      left_join(dfKingdom,by="AphiaID")
    
    if(!is.null(input$Kingdom)){
      if(!input$Kingdom==sAll){
        df1<-df1[df1$Kingdom == input$Kingdom,]
      }
    }
    sMethodSelected<-isolate(input$Method)
    if(!is.null(sMethodSelected)){
      if(!sMethodSelected==sAll){
        df1<-df1[df1$Method == sMethodSelected,]
      }
    }
    
    #sd
    df1 <- distinct(df1,AphiaID)
    
    df1 <- df1 %>% 
      left_join(dfSpecies,by="AphiaID") %>%
      arrange(ScientificName)
    
    return(df1$ScientificName)
    })
  
  
  
  output$mymap <- renderLeaflet({
    
    mapdf<-df_r()
    shp <- shp_r()
    
    

    mapdf <- mapdf %>%
      filter(!is.na(Lat))
    
    pal<-colorFactor(map_method_cols, mapdf$Method,levels=sMethod)

        map<- leaflet({mapdf}) %>% 
      addProviderTiles("Esri.WorldStreetMap", options = providerTileOptions(noWrap = TRUE))
      #  %>%
      #setView(zoom=6,lat=56.5,lng=11.5)
    #browser()
        
    if(nrow(shp)>0){
      map <- map %>% addPolygons(data=shp,color = "white", weight = 0.4, smoothFactor = 0.5,
                                 opacity = 1.0, fillOpacity = 0.3,
                                 popup=~YearList,label=~Years, 
                                 #fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
                                 fillColor = map_method_cols[1],
                                 highlightOptions = highlightOptions(color = "red", weight = 2))
                                                                    # bringToFront = TRUE))
    }        
        
    if(nrow(mapdf)>0){
      if(input$grouppoints==T){
        map <- map %>% addCircleMarkers(~Lon, ~Lat, radius=5,popup=~YearList,label=~Years,
                                        stroke=TRUE, color=~pal(Method),weight=1,opacity=1,
                                        fillOpacity = 1,fillColor=~pal(Method),
                                        clusterOptions=markerClusterOptions() )
      }else{
        map <- map %>% addCircleMarkers(~Lon, ~Lat, radius=5,popup=~YearList,label=~Years,
                                        stroke=TRUE, color=~pal(Method),weight=1,opacity=1,
                                        fillOpacity = 1,fillColor=~pal(Method))
        
      }
        
    }
        
    #cat(map)    

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
      selectInput("Kingdom", sLabelKingdom, choices=Kingdomlist,multiple=F) #,width='400px'

    )})
  
  output$SelectMethod <- renderUI({
    tagList(
      selectInput("Method", sLabelMethod, choices=c(sAll,MethodList()))
    )})
  
  output$useGrouping <- renderUI({
    tagList(checkboxInput("grouppoints",sLabelGroup,value=F))
  })
  
  
  output$AppTitle <- renderText(sAppTitle)
  output$disclaimer <- renderText(sDisclaimer)
  output$language <- renderText(sLanguage)
  
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
        sTitle <- paste0(sTitle,", ",input$Region,"")
      }
    }
    if(!is.null(input$Method)){
      if(input$Method!=sAll){
        dfplot<-dfplot[dfplot$Method == input$Method,]
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

    ymax <- max(dfplot$Count,na.rm=T)
    ymax <- max(ymax,4)
   
    
    p <- ggplot(dfplot) + geom_bar(stat="identity",position="stack",
                                   aes(x=Year,y=Count,fill=factor(Method,levels=sMethod)),width=0.8) + 
      theme_minimal() +  scale_fill_manual(values = method_cols,name=sLabelMethod) +
      ggtitle(sTitle) + labs(y = sLabelCount, x=sLabelYear) +
      theme(legend.position = "bottom") +
      scale_y_continuous(breaks= pretty_breaks()) +
      coord_cartesian(ylim = c(0, ymax))
    
    p
    
  })
  
  output$imgLogo <- renderImage({list(src=sLogoFile,alt="NIVA DK logo")},deleteFile=F)
  
  
  
})
