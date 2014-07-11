library(shiny)
library(NPSForVeg)
library(leaflet)
#$logo="<img src='ah_large_black.gif', style='float:right; padding-right:25px'/>",

shinyUI(
  
  
  navbarPage(title="Forest Vegetation Visualizer", #windowTitle="Forest Veg",
  icon="AH_small_flat_4C_12x16.png",inverse=T,
  #echo($logo)
  #img(src="ah_large_black.gif", style="float:right; padding-right:25px"),
 
  ######################################### Map Panel ####################################################################
    tabPanel(title="Map",
      div(class="outer",
        tags$head(
          includeCSS("mapstyles.css")
        ),
        leafletMap("map", width="100%", height="100%",
          initialTileLayer="//{s}.tiles.mapbox.com/v3/nps.2yxv8n84/{z}/{x}/{y}.png",
          initialTileLayerAttribution = HTML("&copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a> 
          &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors | 
          <a class='improve-park-tiles' href='http://www.nps.gov/npmap/park-tiles/improve/' 
                                             target='_blank'>Improve Park Tiles</a>"),
          options=list(
            center = c(39.03, -77.80),
            zoom = 9,
            maxBounds = list(list(37.70,-79.5), list(40.36,-76.1)), # Show NCRN only
            minZoom=8
          )
        ),
################### Main Map Controls 
        conditionalPanel(condition="input.ShowControls",
          fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top=50,bottom="auto",height="auto",right=20, 
                     left="auto", width=200,
            h3("Forest Explorer"),
            selectInput(inputId="MapGroup", label="Type of plant:",
                      choices=c(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings",Shrubs="shrubs",
                                "Shrub seedlings"="shseedlings","Understory plants"="herbs","Vines on Trees"="vines")),
            uiOutput("PlantValueControl"),
       # sliderInput(inputId="MapYear", label="Display data from the 4 years ending:", min=2009, max=2013, 
                #value=2013, format="####",width="150px"),
            selectizeInput(inputId="MapYear", label="Display data from the 4 years ending:", 
                           choices=c(2009:2013),selected=2013 ),
                            
            tags$div(title="Choose a species of plants to map",
              uiOutput("MapSpeciesControl")
            ),  
            uiOutput("MapParkControl")
          )
       ),
############### Add a layer control
        conditionalPanel(condition="input.ShowLayers",
          fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top="80%",bottom="auto"
                     ,height="auto",right=20,left="auto",width=200,
            h4("Map Layers"),
            selectizeInput(inputId="MapLayer", label="Add a map layer:", 
                           choices=c(None="None", "Forested Areas"="ForArea","Soil Map (slow)"="SoilMap"))
          )
        ),

########################## Zoom  Control
        conditionalPanel(condition="input.ShowZoom",
          fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top=50,bottom="auto",height="auto",
                     left=350,width=200,
            h4("Zoom to:"),
            uiOutput("ParkZoomControl"),
            actionButton(inputId="MapZoom", label="Go",icon=icon("search-plus")),
            hr(),
            tags$div(title="Increases size of plots for easier viewing",
                 radioButtons(inputId="PlotSize", label="Magnify plots: 1X = to scale", 
                              choices=c("1X"="1", "4X"="2", "9X"="3", "16X"="4"), selected="1", inline=TRUE)
            )
          )
        ),

##################### Map Legend
        conditionalPanel(condition="input.ShowPlots",
          fixedPanel( id="controls", class="modal", draggable=TRUE, cursor="auto", top=50, bottom="auto", height=200,
              right=300, left="auto", width=150,
            h4(uiOutput("MapLegendTitle")),
            uiOutput("MapLegend")
          )
        ),
################Layer Legend

      conditionalPanel(
        condition="input.MapLayer!='None' & input.ShowLayerLegend",
        fixedPanel( id="controls", class="modal", draggable=TRUE, cursor="auto", top=325, bottom="auto", height="auto",
                    right="auto", left=350, width=150,
                    h4(uiOutput("LayerLegendTitle")),
                    uiOutput("LayerLegend")
      )),
############## Show hide Panel
      absolutePanel(id="controls", class="modal", draggable=TRUE, cursor="auto",top="95%", height=15, left=350, width=600,
                flowLayout(
                  strong("Show:"),
                  checkboxInput(inputId="ShowControls", label="Map Controls", value=TRUE),
                  checkboxInput(inputId="ShowPlots", label="Plot Legend", value=TRUE),
                  checkboxInput(inputId="ShowZoom", label="Zoom", value=TRUE),
                  checkboxInput(inputId="ShowLayers", label="Map Layers", value=TRUE),
                  checkboxInput(inputId="ShowLayerLegend", label="Layer legend", value=TRUE)
                )
      )


    ) ## end of map div
  ),  ## end of map page




######################################## Graphs Panel ##########################################################

    navbarMenu("Plots",    
#############  densplot() based plots
      tabPanel(title="Data by Park and Species",    
        fluidRow(
          column(3,
            wellPanel(
              h4("Base Data:"),
              uiOutput(outputId="densParkControl"),
              sliderInput(inputId="densYear", label="Display data from the 4 years ending:", 
                          min=2009, max=2013, value=2013, format="####"),
              hr(),
              tags$div(title="Choose the type of plant you want to work with", 
                selectizeInput(inputId="densGroup", label="Type of plant:",   choices=c(Trees="trees",
                    Saplings="saplings","Tree seedlings"="seedlings",Shrubs="shrubs",
                    "Shrub seedlings"="shseedlings","Understory plants"="herbs","Vines on Trees"="vines"))
              ),
              radioButtons(inputId="densSpeciesType", label="Which species?", 
                choices=c("Most common species"="Common","Pick individual species"="Pick",
                          "All species combined"="All"),inline=TRUE
              ),
              br(),
              checkboxInput(inputId="densCommon", label="Show common names?", value=FALSE ),
              hr(),
              uiOutput(outputId="densSpeciesControl"),
              hr(),
              uiOutput(outputId="densValControl"),
              hr(),
              h4("Comparison Data:"),
              radioButtons(inputId="CompareType", label ="Compare to another:",
                choices=c("None","Park","Growth Stage","Time"),selected="None",inline=TRUE
              ),
              uiOutput(outputId="CompareSelect"),
              h1(br(),br(),br(),br(),br())
            )
          ),
          column(9,
            tabsetPanel(type="pills",
              tabPanel(title="Plot",
                tags$div(title="Mean and 95% Confidence interval",plotOutput(outputId="DensPlot", height="600px"))
              ),
              tabPanel(title="Data",
                dataTableOutput("densTable")
              ),
              tabPanel(title="About this graph...",
                h4("Add explanation of graph here")
              )
            )
          )
        )
      ),

###############IV Plots
      tabPanel(title="Forestry Importance Values (IV)",
        fluidRow(
          column(3,
            wellPanel(
              uiOutput("IVParkControl"),
              br(),
              tags$div(title="Choose the type of plant you want to work with", 
                selectizeInput(inputId="IVGroup", label="Type of plant:",choices=c(Trees="trees",Saplings="saplings",
                    "Tree seedlings"="seedlings","Shrub seedlings"="shseedlings","Understory plants"="herbs"))
              ),
              br(),
              sliderInput(inputId="IVYear", label="Display data from the 4 years ending:", min=2009, max=2013,
                          value=2013, format="####"),
              br(),
              checkboxInput(inputId="IVPart", label="Show Components of the Importance Value?", value=FALSE),
              br(),
              checkboxInput(inputId="IVCommon", label="Show common names?", value=FALSE),
              br(),
              sliderInput(inputId="IVTop",label="Number of species to plot (in order of IV):",min=1, max=20,
                          value=10, format="##",ticks=FALSE)
            )
          ),
          column(9,
            tabsetPanel(type="pills",
              tabPanel("Plot",
                plotOutput("IVPlot")
              ),
              tabPanel(title="Data",
                dataTableOutput("IVData")
              ),
              tabPanel(title="About this graph...",
                includeHTML(paste0(getwd(),"/www/","IVPlot.html"))
              )
            )
          )
        )
      )
    ),

################################# Project Information
    tabPanel("Project Informaiton",
      h3("Add some words here")
    ),



################ Citations 

    tabPanel("Citations and References",
      h3("Words and links here")
    )
))#end navabarPage() and shinyUI()