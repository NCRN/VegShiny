library(shiny)
library(NPSForVeg)
library(leaflet)


shinyUI(
  ### fluid page is only there to get the icon in the browser tab.
  fluidPage(list(tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png",type="image/png" />'))),
  navbarPage(
    
    windowTitle="Forest Vegetation",
    #icon="./www/AH_small_flat_4C_12x16.png", #restore this on 10.1
    title=HTML("<div> <img src='ah_small_black.gif',alt='Forest Vegetation Visualizer'>
              Forest Vegetation Visualizer</div>"),
 
    inverse=T,


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
          fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top=70,bottom="auto",height="auto",right=20, 
                     left="auto", width=200,
            h4("Data to Map"),
            tags$div(title="Choose the type of plant you want to work with", selectInput(inputId="MapGroup", 
              label="Type of plant:", choices=c(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings",
                Shrubs="shrubs", "Shrub seedlings"="shseedlings","Understory plants"="herbs","Vines on Trees"="vines"))),
            tags$div(title="Type of data to map",uiOutput("PlantValueControl")),
            tags$div(title="Choose the four year period you want to work with.", sliderInput(inputId="MapYear", 
              label="Display data from the 4 years ending:", min=2009, max=2013,value=2013, format="####",width="150px")),
            tags$div(title="Choose a species of plants to map", uiOutput("MapSpeciesControl")),  
            tags$div(title="Filter the species list so only species found in a particular park are listed",
                     uiOutput("MapParkControl"))
          )
       ),
############### Add a layer control
        tags$div(title="Overlay additional data onto the parks",
          conditionalPanel(condition="input.ShowLayers",
            fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top="80%",bottom="auto"
                     ,height="auto",right=20,left="auto",width=200,
            h4("Map Layers"),
            selectizeInput(inputId="MapLayer", label="Add a map layer:", 
                        choices=c(None="None", "Forested Areas"="ForArea","Soil Map (slow)"="SoilMap")))
          )
        ),

########################## Zoom  Control
        conditionalPanel(condition="input.ShowZoom",
          fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top=70,bottom="auto",height="auto",
                     left=350,width="210",
            h4("Zoom to:"),
            tags$div(title="Choose a park and click 'Go'", uiOutput("ParkZoomControl"),
            actionButton(inputId="MapZoom", label="Go",icon=icon("search-plus"))),
            hr(),
            tags$div(title="Increases size of plots for easier viewing",
                 radioButtons(inputId="PlotSize", label="Magnify plots: 1X = to scale", 
                              choices=c("1X"=1, "5X"=sqrt(5), "10X"=sqrt(10), "25X"=5), selected="1", inline=TRUE)
            )
          )
        ),

##################### Map Legend
        conditionalPanel(condition="input.ShowPlots",
          fixedPanel( id="controls", class="modal", draggable=TRUE, cursor="auto", top=70, bottom="auto", height="auto",
              right=300, left="auto", width="auto",
            h4(uiOutput("MapLegendTitle")),
            uiOutput("MapLegend")
          )
        ),
################Layer Legend

      conditionalPanel(
        condition="input.MapLayer!='None' & input.ShowLayerLegend",
        fixedPanel( id="controls", class="modal", draggable=TRUE, cursor="auto", top=325, bottom="auto", height="auto",
                    right="auto", left=350, width="auto",
                    h4(uiOutput("LayerLegendTitle")),
                    uiOutput("LayerLegend")
      )),
############## Show hide Panel
      tags$div(title="Choose to show or hide panels",
        absolutePanel(id="controls", class="modal", draggable=TRUE, cursor="auto",top="95%", height=15, 
                      left=350, width=600,
          flowLayout(
            strong("Show:"),
            checkboxInput(inputId="ShowControls", label="Map Controls", value=TRUE),
            checkboxInput(inputId="ShowPlots", label="Plot Legend", value=TRUE),
            checkboxInput(inputId="ShowZoom", label="Zoom", value=TRUE),
            checkboxInput(inputId="ShowLayers", label="Map Layers", value=TRUE),
            checkboxInput(inputId="ShowLayerLegend", label="Layer legend", value=TRUE)
          )
        )
      )
    ) ## end of map div
  ),  ## end of map page




######################################## Graphs Panel ##########################################################

    navbarMenu(tags$div(title="Graph the data","Graphs"),    
#############  densplot() based plots
      tabPanel(tags$div(title="Graph abundance, basal area, percent cover, &c.","Data by Park and Species"),   
        fluidRow(
          column(3,
            wellPanel(
              h4("Base Data:"),
              tags$div(title="Choose a park to work with.",
                       uiOutput(outputId="densParkControl")
              ),
              tags$div(title="Pick the four year period you want to graph",
                       sliderInput(inputId="densYear", label="Display data from the 4 years ending:", 
                          min=2009, max=2013, value=2013, format="####")
              ),
              hr(),
              tags$div(title="Choose the type of plant you want to work with", 
                selectizeInput(inputId="densGroup", label="Type of plant:",   choices=c(Trees="trees",
                    Saplings="saplings","Tree seedlings"="seedlings",Shrubs="shrubs",
                    "Shrub seedlings"="shseedlings","Understory plants"="herbs","Vines on Trees"="vines"))
              ),
              tags$div(title="Graph the most common species, species you choose, or all species combined.",
                radioButtons(inputId="densSpeciesType", label="Which species?", 
                choices=c("Most common species"="Common","Pick individual species"="Pick",
                          "All species combined"="All"),inline=TRUE)
              ),
              hr(),
              uiOutput(outputId="densSpeciesControl"),
              hr(),
              tags$div(title="Toggle between common and scientific names",
                checkboxInput(inputId="densCommon", label="Show common names?", value=FALSE )
              ),
              br(),
              tags$div(title="Type of data to graph",
                uiOutput(outputId="densValControl")
              ),
              hr(),
              h4("Comparison Data:"),
              tags$div(title="Comare the base data with a differnet park, growth stage, or time period",
                radioButtons(inputId="CompareType", label ="Compare to another:",
                choices=c("None","Park","Growth Stage","Time"),selected="None",inline=TRUE)
              ),
              uiOutput(outputId="CompareSelect"),
              h1(br(),br(),br(),br(),br())
            )
          ),
          column(9,
            tabsetPanel(type="pills",
                tabPanel(tags$div(title="Graph the data", "Graph"),
                tags$div(title="Mean and 95% Confidence interval",plotOutput(outputId="DensPlot", height="600px"))
              ),
              tabPanel(tags$div(title="See all data in a table","Data"),
                dataTableOutput("densTable")
              ),
              tabPanel(tags$div(title="Explanation of the graph","About this graph..."),
                       includeHTML(paste0(getwd(),"/www/","DensPlot.html"))
              )
            )
          )
        )
      ),

###############IV Plots
      tabPanel(tags$div(title="Graph IV", "Forestry Importance Values (IV)"),
        fluidRow(
          column(3,
            wellPanel(
              tags$div(
                title="Choose a park to work with.",
                uiOutput("IVParkControl")
              ),
              br(),
              tags$div(
                title="Choose the type of plant you want to work with", 
                selectizeInput(inputId="IVGroup", label="Type of plant:",choices=c(Trees="trees",Saplings="saplings",
                    "Tree seedlings"="seedlings","Shrub seedlings"="shseedlings","Understory plants"="herbs"))
              ),
              br(),
              tags$div(
                title="Pick the four year period you want to graph",
                sliderInput(inputId="IVYear", label="Display data from the 4 years ending:", min=2009, max=2013,
                          value=2013, format="####")
              ),
              br(),
              tags$div(
                title="Show density, size and disbribution separately",
                checkboxInput(inputId="IVPart", label="Show Components of the Importance Value?", value=FALSE)
              ),
              br(),
              tags$div(
                title="Toggle between common and scientific names",
                checkboxInput(inputId="IVCommon", label="Show common names?", value=FALSE)
              ),
              br(),
              tags$div(
                title="Chose the maximum number of species to display.",
                sliderInput(inputId="IVTop",label="Number of species to plot (in order of IV):",min=1, max=20,
                  value=10, format="##",ticks=FALSE)
              )
            )
          ),
          column(9,
            tabsetPanel(type="pills",
              tabPanel(tags$div(title="Graph the data",
                  "Graph"
                ),
                tags$div(title="Graph of IV",plotOutput("IVPlot"))
              ),
              tabPanel(tags$div(title="See all data in a table",
                  "Data"
                ),
                h3("Importance Values for all Species Monitored"),
                dataTableOutput("IVData")
              ),
              tabPanel(tags$div(title="Explanation of the graph",
                  "About this graph..."
                ),
                includeHTML(paste0(getwd(),"/www/","IVPlot.html"))
              )
            )
          )
        )
      )
    ),

################################# Project Information
    tabPanel("Project Information",
      h3("Add some words here")
    ),



################ Citations 

    tabPanel("Citations and References",
      h3("Words and links here")
    )
)#end navbarPage()
)#end fluidPage()
)#end  shinyUI()