

library(shiny)
library(leaflet)

shinyUI(
  fluidPage(title="NISAR",
            titlePanel(title=div(
              a(href="https://niva-denmark.dk/",
                img(src="NIVA-Denmark-150.png"))
              ,textOutput("AppTitle")  # "Kort over ikke-hjemmehørende arter"
            )),
            
            column(3,
                   wellPanel(
                     uiOutput("SelectKingdom"),
                     uiOutput("SelectSpecies"),
                     uiOutput("SelectRegion"),
                     uiOutput("SelectMethod"),
                     tableOutput("table")
                   )
            ),
            column(4,
                   leafletOutput("mymap",height = 500)
            ),
            column(4,plotOutput("barPlot")
                   
            )
            
            
  ))
