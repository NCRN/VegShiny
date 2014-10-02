library(shiny)
library(NPSForVeg)
library(leaflet)
library(shinyBS)

shinyUI(

  
  
  navbarPage(
    

    windowTitle="Forest Vegetation",
    #icon="AH_small_flat_4C_12x16.png", #this does not work on Shiny 10.1
    title=HTML("<div> <img src='ah_small_black.gif', alt='Forest Vegetation Visualizer'> Forest Vegetation Visualizer</div>"),
    inverse=T,

   
  ######################################### Map Panel ####################################################################
    tabPanel(title="Map",
      tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />')), #puts up icon on tab
      tags$head(includeScript("./www/forveg-analytics.js")),  
  
  
##### About the Map modal goes here so it does not get caught in the "outer" div below here it has css problems
      bsModal(id="MapInfoModal", title="About the Map",,  role="dialog", trigger="AboutMapButton", href="AboutMap.html" ),

      div(class="outer",
        tags$head(includeCSS("./www/mapstyles.css") ), # defines css file
        


        
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
            h4("Map Controls"),
            tags$div(title="Choose the type of plant you want to work with", selectInput(inputId="MapGroup", 
              label="Type of plant:", choices=c(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings",
                Shrubs="shrubs", "Shrub seedlings"="shseedlings","Understory plants"="herbs","Vines on Trees"="vines"))),
            tags$div(title="Type of data to map",uiOutput("PlantValueControl")),
            tags$div(title="Choose the four year period you want to work with.", sliderInput(inputId="MapYear", 
              label="Display data from the 4 years ending:", min=2009, max=2013,value=2013, format="####",width="150px")),
            tags$div(title="Toggle between common and scientific names",
                     checkboxInput(inputId="mapCommon", label="Show common names?", value=TRUE )),
            tags$div(title="Choose a species of plants to map", uiOutput("MapSpeciesControl")),  
            tags$div(title="Filter the species list so only species found in a particular park are listed",
                     uiOutput("MapParkControl")),
            bsButton(inputId="AboutMapButton",label="About the map...",style="primary")
          )
       ),

############### Map Modal 


############### Add a layer control
        tags$div(title="Overlay additional data onto the parks",
          conditionalPanel(condition="input.ShowLayers",
            fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top="60%",bottom="auto"
                     ,height="auto",right="auto",left=350,width=200,
            h4("Map Layers"),
            selectizeInput(inputId="MapLayer", label="Add a map layer:", 
                        choices=c(None="None", "EcoRegions"="EcoReg","Forested Areas"="ForArea"
                                  #,"Soil Map (slow)"="SoilMap"
                                  )))
          )
        ),

########################## Zoom  Control
        conditionalPanel(condition="input.ShowZoom",
          fixedPanel(id="controls",class="modal",draggable=TRUE,cursor="auto",top=70,bottom="auto",height="auto",
                     left=350,width=210,
            h4("Zoom to:"),
            tags$div(title="Choose a park and click 'Go'", uiOutput("ParkZoomControl"),
            actionButton(inputId="MapZoom", label="Go")),
            hr(),
            tags$div(title="Increases size of plots for easier viewing",
                 radioButtons(inputId="PlotSize", label="Enlarge plots: 1X = to scale", 
                              choices=c("1X"=1, "5X"=sqrt(5), "10X"=sqrt(10), "25X"=5), selected="1", inline=TRUE)
            )
          )
        ),

##################### Map Legend
        conditionalPanel(condition="input.ShowPlots",
          fixedPanel( id="controls", class="modal", draggable=TRUE, cursor="auto", top=70, bottom="auto", height="auto",
              right=300, left="auto", width=130,
            h4("Legend"),
            uiOutput("MapLegendTitle"),
            uiOutput("MapLegend")
          )
        ),
################Layer Legend

      conditionalPanel(
        condition="input.MapLayer!='None' & input.ShowLayerLegend",
        fixedPanel( id="controls", class="modal", draggable=TRUE, cursor="auto", top=325, bottom="auto", height="auto",
                    right="auto", left=350, width="auto",
                    h4("Layer Legend"),
                    strong(uiOutput("LayerLegendTitle")),
                    uiOutput("LayerLegend")
      )),
############## Show hide Panel
      tags$div(title="Choose to show or hide panels",
        absolutePanel(id="controls", class="modal", draggable=TRUE, cursor="auto",top="95%", height=15, 
                      left=350, width=600,
          flowLayout(
            strong("Show:"),
            checkboxInput(inputId="ShowControls", label="Map Controls", value=TRUE),
            checkboxInput(inputId="ShowPlots", label="Legend", value=TRUE),
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
      tabPanel(tags$div(title="Graph abundance, basal area, percent cover, etc.","Data by Park and Species"),   
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
              tags$div(title="Toggle between common and scientific names",
                       checkboxInput(inputId="densCommon", label="Show common names?", value=TRUE )
              ),
              br(),
              tags$div(title="Graph the most common species, species you choose, or all species combined.",
                radioButtons(inputId="densSpeciesType", label="Which species?", 
                choices=c("Most common species"="Common","Pick individual species"="Pick",
                          "All species combined"="All"),inline=TRUE)
              ),
              hr(),
              uiOutput(outputId="densSpeciesControl"),
              hr(),
              tags$div(title="Type of data to graph",
                uiOutput(outputId="densValControl")
              ),
              conditionalPanel(
                condition="input.densPanel=='Graph'",
                hr(),
                flowLayout(
                  bsButton(inputId="densGraphButton", label="Display Options",style="primary"),
                  downloadButton(outputId="densGraphDownload", label="Save Graph (.jpg)", class="btn btn-primary"),
                  downloadButton(outputId="densWmfDownload", label="Save graph (.wmf)", class="btn btn-primary")
                ),
                hr(),
                h4("Comparison Data:"),
                tags$div(title="Compare the base data with a differnet park, growth stage, or time period",
                  radioButtons(inputId="CompareType", label ="Compare to another:",
                  choices=c("None","Park","Growth Stage","Time"),selected="None",inline=TRUE)
                ),
                uiOutput(outputId="CompareSelect"),
                h1(br(),br(),br(),br(),br())
              ),
              conditionalPanel(
                condition="input.densPanel=='Table'",
                hr(),
                downloadButton(outputId="densTableDownload", label="Save Table (.csv)", class="btn btn-primary")
              )
            )
          ),
          column(9,
            tabsetPanel(id="densPanel",type="pills",
                tabPanel(title=tags$div(title="Graph the data", "Graph"),value="Graph",
                  tags$div(title="Mean and 95% Confidence interval",plotOutput(outputId="DensPlot", height="600px")),
                  bsModal(id="DensModal", title="Display Options", trigger="densGraphButton",
                    tags$head(tags$style(HTML("#DensModal{ width:350px; background-color:rgba(255,255,255, 0.8)} 
                                              #DensModal:hover {background-color:rgba(255,255,255, 1)}
                                              #DensModal .modal-body{height:150px; overflow:visible}
                                              #DensModal .modal-footer{background-color:rgba(245,245,245,0.5)} "))),
                    flowLayout(
                      selectizeInput("densBaseColor","Base Data Color:",choices=ColorNames, selected="blue",width="125px"),
                      selectizeInput("densCompareColor","Comparison Data Color:",choices=ColorNames, selected="red",width="125px")
                    ),
                    br(),
                    flowLayout(
                      sliderInput("densPointSize", "Change Point Size", min=4, max=24, value=8, step=2,width="125px"),
                      sliderInput("densFontSize", "Change Font Size", min=12, max=32, value=20, step=2,width="125px")
                    )
                  )
                ),
              tabPanel(
                tags$div(title="See all data in a table","Data table"),
                value="Table",
                column(10,
                 h3(textOutput("densTableTitle")),
                  dataTableOutput("densTable")
                )
              ),
              tabPanel(tags$div(title="Explanation of the graph","About this graph..."),
                       includeHTML("./www/DensPlot.html")
              )
            )
          )
        )
      ),

###############IV Plots
      tabPanel(tags$div(title="Graph Importance Values", "Forestry Importance Values (IV)"),
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
                    "Tree seedlings"="seedlings","Shrub seedlings"="shseedlings"))
              ),
              br(),
              tags$div(
                title="Toggle between common and scientific names",
                checkboxInput(inputId="IVCommon", label="Show common names?", value=TRUE)
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
                title="Chose the maximum number of species to display.",
                sliderInput(inputId="IVTop",label="Number of species to plot (in order of IV):",min=1, max=20,
                  value=10, format="##",ticks=FALSE)),
                conditionalPanel(
                  condition="input.IVPanel=='Graph'",
                  hr(),
                  flowLayout(
                    bsButton(inputId="IVGraphButton", label="Display Options", style="primary"),
                    downloadButton(outputId="IVGraphDownload", label="Save Graph (.jpg)", class="btn btn-primary")
                  )),
                  conditionalPanel(
                    condition="input.IVPanel=='Table'",
                    hr(),
                    flowLayout(
                      downloadButton(outputId="IVTableDownload", label="Save Table (.csv)", class="btn btn-primary")
                    )
                )
            )
          ),
          column(9,
            tabsetPanel(id="IVPanel",type="pills",
              tabPanel(value="Graph",
                tags$div(title="Graph the data","Graph"),
                tags$div(title="Graph of IV",plotOutput("IVPlot",height="600px")),
                bsModal(id="IVModal", title="Display Options", trigger="IVGraphButton",
                        tags$head(tags$style(HTML("#IVModal {width:475px; background-color:rgba(255,255,255, 0.8)} 
                                                  #IVModal:hover {background-color:rgba(255,255,255, 1)}
                                                  #IVModal .modal-body {height:200px; overflow:visible}
                                                  #IVModal .modal-footer{background-color:rgba(245,245,245,0.5)}"))),
                        flowLayout(
                          selectizeInput("IVBaseColor","Base Color:",choices=ColorNames, selected="green4",width="125px"),
                          sliderInput("IVFontSize", "Change Font Size", min=10, max=24, value=14, step=2,width="175px")
                        ),
                        h5("Component Colors:"),
                        flowLayout(
                          selectizeInput("IVDensityColor","Density Color:",choices=ColorNames, selected="green4", width="125px"),
                          selectizeInput("IVSizeColor","Size Color:",choices=ColorNames, selected="chartreuse",width="125px"),
                          selectizeInput("IVDistributionColor","Distribution Color:",choices=ColorNames, selected="yellow",
                                       width="125px")
                        )
                )
              ),
              tabPanel(value="Table",
                tags$div(title="See all data in a table","Data table"),
                column(10,
                  h3(textOutput("IVTableTitle")),
                  dataTableOutput("IVData")
                )
              ),
              tabPanel(tags$div(title="Explanation of the graph",
                  "About this graph..."
                ),
                includeHTML("./www/IVPlot.html")
              )
            )
          )
        )
      )
    ),

############################## Species Lists
    tabPanel(id="SpeciesPanel",
      tags$div(
        title="Lists of plants found in the parks", "Species Lists"
      ),
      column(3,id="SpeciesControls",
        tags$head(tags$style(HTML("#SpeciesControls{height:400px}"))),
        wellPanel(
          tags$div(
            title="Choose a park to work with.",uiOutput("SpListParkControl")
          )
        )
      ),
      column(9,
        h3("Species Found in the Monitoring Plots"),
        dataTableOutput("SpeciesTable")
      )
    ),

##################### About
    navbarMenu(
      tags$div(
        title="About the project", "About"
        ),

################################# Project Information
    tabPanel(
      tags$div(
        title="Background Informaiton", "Project Information"
      ),
      includeHTML("./www/Information.html")
    ),



################ Citations 

    tabPanel("Citations & References",
     includeHTML("./www/Citations.html")
    )
) #end About menu
)#end navbarPage()
)#end  shinyUI()