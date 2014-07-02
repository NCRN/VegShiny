library(shiny)
library(NPSForVeg)
library(leaflet)


shinyUI(
  navbarPage(title="Forest Vegetation Visualizer", windowTitle="Forest Veg",
  
######################################### Map Panel ####################################################################
    tabPanel("Map",
      div(class="outer",
        tags$head(
          includeCSS("mapstyles.css")
        ),
        leafletMap("map", width="100%", height="100%",initialTileLayer="//{s}.tiles.mapbox.com/v3/nps.2yxv8n84/{z}/{x}/{y}.png",
          initialTileLayerAttribution = HTML("&copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a> 
          &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors | 
          <a class='improve-park-tiles' href='http://www.nps.gov/npmap/park-tiles/improve/' target='_blank'>Improve Park Tiles</a>"),
        options=list(
          center = c(39.03, -77.80),
          zoom = 9,
          maxBounds = list(list(37.70,-79.5), list(40.36,-76.1)), # Show NCRN only
          minZoom=8
        )
      ),

################### Main Map Controls 
      fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top=50,bottom="auto",height="auto",right=20,left="auto",width=200,
        h3("Forest Explorer"),
        
        selectInput(inputId="MapGroup", label="Type of plant:",
          choices=c(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings",Shrubs="shrubs","Shrub seedlings"="shseedlings",
            "Understory plants"="herbs","Vines on Trees"="vines")),
                            
        uiOutput("PlantValueControl"),
                            
        tags$div(title="Choose a species of plants to map",
          uiOutput("MapSpeciesControl")
        ),  
        uiOutput("MapParkControl"),
        hr(),
        tags$div(title="Increases size of plots for easier viewing",
          radioButtons(inputId="PlotSize", label="Magnify plots: 1X=to scale", choices=c("1X"="1", "4X"="2", "9X"="3", "16X"="4"), 
                       selected="1", inline=TRUE)
        ),
        hr(),
        checkboxInput(inputId="ShowMap", label="Show Soil Map", value=FALSE)
      ),

########################## Zoom  Control
      fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top=50,bottom="auto",height="auto", left=350,width=175,
        h4("Zoom to:"),
        uiOutput("ParkZoomControl"),
        actionButton(inputId="MapZoom", label="Go",icon=icon("search-plus"))
      ),

##################### Map Legend
      fixedPanel( id="controls", class="floater",style="", draggable=TRUE, cursor="auto", top=50, bottom="auto", height=200,
            right=300, left="auto", width=150,
        uiOutput("MapLegendTitle"),
        uiOutput("MapLegend")
      ) 
    )
  ),




######################################## Graphs Panel ##########################################################
    tabPanel("Graphs",    
      fluidRow(
        column(3,wellPanel(
          h4("Base Data:"),
          uiOutput(outputId="ParkControl"),
          sliderInput(inputId="DensYearIn", label="Display data from the 4 years ending:", min=2009, max=2013, value=2013, format="####"),
          hr(),
          tags$div(title="Choose the type of plant you want to work with", selectizeInput(inputId="densgroup", label="Type of plant:",   
                choices=c(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings",Shrubs="shrubs","Shrub seedlings"="shseedlings",
                "Understory plants"="herbs","Vines on Trees"="vines"))),
          
          radioButtons(inputId="SpeciesType", label="Which species?", 
                       choices=c("Most common species"="Common","Pick individual species"="Pick", "All species combined"="All"), inline=TRUE),
          hr(),
          uiOutput(outputId="DensSpeciesControl"),
          hr(),
          uiOutput(outputId="DensValControl"),
          hr(),
          h4("Comparison Data:"),
          radioButtons(inputId="CompareType", label ="Compare to another:", choices=c("None","Park","Growth Stage","Time"),
                       selected="None",inline=TRUE),
          uiOutput(outputId="CompareSelect"),
          h1(br(),br(),br(),br(),br())
        )),
        column(9,
          tags$div(title="Mean and 95% Confidence interval",plotOutput("DensPlot"))
        )
      )
    )
))