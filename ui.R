

library(shiny)
library(leaflet)
library(shinyBS)

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
                   bsCollapse(id = "collapseExample", open = NULL,
                              bsCollapsePanel("Figurforklaring",
                                              p("Observationer er vist som punkter på kortet.",
                                                "Farven indikerer om arten er registret",
                                                " med konventionelle metoder (blå)",
                                                img(src="key20_1.png"),
                                                "eller med eDNA (rød)",
                                                img(src="key20_2.png"),
                                                ". Grønt indikerer at arten blev fundet med begge type metoder",
                                                img(src="key20_12.png"),"."
                                              ),
                                              p("For mange fiskearter er data 'anonymiseret' sådan at det præcise punkt for",
                                                "registreringen ikke kan ses. Her vises den 10 x 10 km",
                                                "gridcelle hvor arten er fundet.",
                                                img(src="key20_3.png")),
                                              p("Når musepilen placeres over et punkt, vises en 'popup'",
                                                "der indikerer den tidligste og seneste årstal hvor arten",
                                                "er registreret det pågældende sted. Hvis der kun er",
                                                "registreret fund i et enkelt år, så vises kun dette år i 'popup'en."),
                                                p(img(src="key_mouseover.png")),
                                              p("Hvis man klikker på punktet, kommer der en liste frem med alle år", 
                                                "hvor arten er registreret."),
                                              p(img(src="key_popup.png")))),              
                   tags$br() # spacer 
                   
            ),
            column(4,plotOutput("barPlot"),
                   tags$b(),
                   tags$small(htmlOutput("disclaimer"))

            )
            
            
  ))
