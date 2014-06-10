

library(shiny)
library(NPSForVeg)

shinyUI(

  navbarPage(title="Forest Vegetation Visualizer", windowTitle="Forest Veg",
             
    tabPanel("Map",
             div(class="outer",
                 tags$head(
                   includeCSS("mapstyles.css")
                   ),
                 
                 leafletMap("map", width="100%", height="100%",
                            initialTileLayer="//{s}.tiles.mapbox.com/v3/nps.2yxv8n84,nps.jhd2e81b/{z}/{x}/{y}.png",
                            initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                            options=list(
                              center = c(39.03, -77.80),
                              zoom = 9,
                              maxBounds = list(list(37.70,-79.5), list(40.36,-76.1)), # Show NCRN only
                              minZoom=8
                            )
                 ),
                 
                 fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top=50,bottom="auto",height="auto",right=20, 
                            left="auto",width=200,
                            h3("Forest Explorer"),
                            
                            selectInput(inputId="MapGroup", label="Type of plant:",
                                                  choices=c("trees","saplings","seedlings","herbs")),
                            
                            tags$div(title="Choose a species of plants to map",
                                     uiOutput("MapSpeciesControl")),

                            
                            uiOutput("MapParkControl")
            
                            
                 )
                 
    )),
    tabPanel("Graphs",         
             
    
    
             
    fluidRow(
      column(3,wellPanel(
          uiOutput(outputId="ParkControl"),
          hr(),
          br(),
          sliderInput(inputId="YearIn", label="Display data from the 4 years ending:", min=2009, max=2013, value=2013, format="####"),
          sliderInput(inputId="TopIn", label="Number of species to display (in order of mean value):", min=1, max=10, value=5, format="##"),
         hr(),
         br(),
          radioButtons(inputId="densvalues", label="Type of data to display", 
                       choices=list("Abundance"="count","Size"="size","Occupancy"="presab") ),
         hr(),
         br(),
         tags$div(title="Choose the type of plant you want to work with",
                 radioButtons(inputId="densgroup", label="Type of plant",
                       choices=c("trees","saplings","seedlings")))
          
    )),

    column(9,

          tags$div(title="Mean and 95% Confidence interval",plotOutput("Testdens"))
    )
    )
)))