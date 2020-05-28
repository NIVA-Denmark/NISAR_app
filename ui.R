

library(shiny)
library(leaflet)

shinyUI(
  fluidPage(title="NISAR",
            titlePanel(title=div(
              a(href="https://niva-danmark.dk/",
                target="blank",
                #img(src="NIVA-Danmark-150.png"))
                imageOutput("imgLogo",width="150px",height="63px",inline=T)),
              tags$b(textOutput("AppTitle"))  # "Kort over ikke-hjemmehørende arter"))
            )),
            
            column(3,
                   wellPanel(
                     uiOutput("SelectKingdom"),
                     uiOutput("SelectSpecies"),
                     uiOutput("SelectRegion"),
                     uiOutput("SelectMethod"),
                     uiOutput("useGrouping"),
                     tableOutput("table")
                   ),
                   tags$br() # spacer 
            ),
            column(4,
                   leafletOutput("mymap",height = 500),
                   tags$br() # spacer 
            ),
            column(4,plotOutput("barPlot"),
                   tags$b(),
                   tags$small(htmlOutput("disclaimer"))
                   #textOutput("disclaimer")  #,a(href="mailto:mail@niva-dk.dk","mail@niva-dk.dk")
            )
            
            
  ))
