library(shiny)
library(NPSForVeg)
library(leaflet)
library(shinyjs)
library(DT)


navbarPage(title=HTML("<div> <a href=",NetworkURL,"> <img src='ah_small_black.gif',
          alt='Forest Vegetation Visualizer'> </a> Forest Vegetation Visualizer</div>"),
    position = "static-top", inverse=TRUE, collapsible = FALSE, fluid=TRUE, windowTitle = paste(Network, "Forest Vegetation"),
    theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", id="MainNavBar",
  ######################################### Map Panel ####################################################################
  
  
   tabPanel(title="Map",style="padding: 0",
     useShinyjs(),

    div(class="outer",
      tags$head(includeCSS("./www/mapstyles.css") ), # defines css file
      tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />')), #puts up icon on tab
      tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
    ),
    
#### Side Control Panel ####    
    fluidRow(
      column(2, style="padding: 0 0 0 10px",
#### Map Controls ####
       div(id="MapControlPanel",class="panel panel-default controls",
            h4("Map Controls", class="panel-heading"),
            tags$div(title="Choose the type of plant you want to work with", selectInput(inputId="MapGroup", 
              label="Type of plant:", choices=PlantTypes)),
            tags$div(title="Choose live or dead", selectInput(inputId="TreeStatus", label="Live or dead",
                                                              choices=c('alive','dead','all'))),
            tags$div(title="Type of data to map",uiOutput("PlantValueControl")),
            tags$div(title="Choose the time period you want to work with.", uiOutput("MapCycleControl")),
            #tags$div(title="Choose the four year period you want to work with.", sliderInput(inputId="MapYear", 
             #     label="Display data from the 4 years ending:", min=Years$Start+Years$Range-1, max=Years$End, value=Years$End,
              #    sep="", step=1,ticks=T)),
            tags$div(title="Toggle between common and scientific names",
                             checkboxInput(inputId="mapCommon", label="Show common names?", value=TRUE )),
            tags$div(title="Choose a species of plants to map", uiOutput("MapSpeciesControl")),  
            tags$div(title="Filter the species list so only species found in a particular park are listed",
                           uiOutput("MapParkControl")),
            actionButton(inputId="AboutMapButton",label="About the map",class="btn btn-primary ")
           ),
        
#### Zoom Controls ####
      div(id="ZoomPanel",class="panel panel-default controls",
          h4("Zoom to:", class="panel-heading"),
          fluidRow(
            column(9, tags$div(title="Choose a park and click 'Go'", uiOutput("ParkZoomControl"))),
            column(3, actionButton(inputId="MapZoom", label="Go", class="btn btn-primary btn-sm"))
          ),
          hr(),
          tags$div(title="Increases size of plots for easier viewing",
           radioButtons(inputId="PlotSize", label="Enlarge plots: 1X = to scale", 
            choices=c("1X"=1, "5X"=sqrt(5), "10X"=sqrt(10), "25X"=5), selected="1", inline=TRUE)
          )
      ),

#### Add a layer control ####
    
      div(id="ExtraLayerPanel",class="panel panel-default controls",draggable=TRUE,cursor="auto",top="60%",bottom="auto",
                          height="auto",right="auto",left=20,width=200,
        h4("Additional Layers", class="panel-heading"),
        tags$div(title="Overlay additional data onto the parks",
           selectizeInput(inputId="MapLayer", label="Add a map layer:", 
                    choices=ExtraLayers))
        )
      ), ## End of controls columns

#### The Map ####
       column(10,style="padding: 0",
           div(leafletOutput("VegMap", height="1000px"))   
       )
     ),
# #### Floating "About the map" Panel ####
hidden(
  fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",top=80,bottom="auto",height="520",
             left=450,width="500",id="AboutMapPanel",style="padding: 0px",
             div(class="panel-heading", h4("About the Map" )),
             div(class="panel-body",style="height: 400px;  overflow-y: scroll",  includeHTML("./www/AboutMap.html")),
             div(class="panel-footer", 
                 actionButton(inputId="CloseAboutMap",class="btn btn-primary",label="Close"))  )
  )
),  ## end of map page


######################################## Graphs Panel ##########################################################

    navbarMenu(tags$div(title="Graph the data","Graphs"),    
#############  densplot() based plots
      tabPanel(tags$div(title="Graph abundance, basal area, percent cover, etc.","Data by Park and Species"), 
               
               useShinyjs(),
        fluidRow(
          column(3,
            wellPanel(class="panel panel-default",
              h4("Data:", class="panel-heading"),
              tags$div(title="Choose a park to work with.",
                       uiOutput(outputId="densParkControl")
              ),
              tags$div(title="Choose the time period you want to work with.", uiOutput("densCycleControl")),
              # tags$div(title="Pick the four year period you want to graph",
              #          sliderInput(inputId="densYear", label="Display data from the 4 years ending:", 
              #                      min=Years$Start+Years$Range-1, max=Years$End, value=Years$End,
              #                      sep="", step=1,ticks=T)
              # ),
              tags$div(title="Choose the type of plant you want to work with", 
                selectizeInput(inputId="densGroup", label="Type of plant:",   choices=PlantTypes)
              ),
              tags$div(title="Toggle between common and scientific names",
                       checkboxInput(inputId="densCommon", label="Show common names?", value=TRUE )
              ),
              tags$div(title="Graph the most common species, species you choose, or all species combined.",
                radioButtons(inputId="densSpeciesType", label="Which species?", 
                choices=c("Most common species"="Common","Pick individual species"="Pick",
                          "All species combined"="All"),inline=FALSE)
              ),
              uiOutput(outputId="densSpeciesControl"),
              tags$div(title="Type of data to graph",
                uiOutput(outputId="densValControl")
              ),
              conditionalPanel(
                condition="input.densPanel=='Graph'",
                actionButton(inputId="densGraphButton", label="Display Options", class="btn btn-primary"),
                div(downloadButton(outputId="densGraphDownload", label="Save Graph (.jpg)", class="btn btn-primary"),
                downloadButton(outputId="densWmfDownload", label="Save Graph (.png)", class="btn btn-primary"))
              ),
              conditionalPanel(
                condition="input.densPanel=='Table'",
                hr(),
                downloadButton(outputId="densTableDownload", label="Save Table (.csv)", class="btn btn-primary")
              )
            ),
            conditionalPanel(
              condition="input.densPanel=='Graph'",
              wellPanel(class="panel panel-default",
                h4("Comparison Data:", class="panel-heading"),
                  tags$div(title="Compare the base data with a differnet park, growth stage, or time period",
                  radioButtons(inputId="CompareType", label ="Compare to another:",
                          choices=c("None","Park","Growth Stage","Time"),selected="None",inline=TRUE)
                  ),
                  uiOutput(outputId="CompareSelect")
              )
            )
          ),
          column(9,
            tabsetPanel(id="densPanel",type="pills",
                tabPanel(title=tags$div(title="Graph the data", "Graph"),value="Graph",
                  tags$div(title="Mean and 95% Confidence interval",
                  plotOutput(outputId="DensPlot", height="600px")),
                 hidden(
                    fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",top=160,bottom="auto",height="auto",
                               left=575,width="auto",id="GraphOptionsPanel",style="padding: 0px",
                      title="Display Options",
                    div(class="panel-heading", h4("Display Options")),
                    div(class="panel-body",
                    flowLayout(cellArgs=list(style="width: 160px"),
                      selectizeInput("densBaseColor","Base Data Color:",choices=ColorNames, selected="blue",width=150),
                      selectizeInput("densCompareColor","Comparison Data Color:",choices=ColorNames, selected="red",width=150)
                    ),
                    br(),
                    flowLayout(cellArgs=list(style="width: 160px"),
                      sliderInput("densPointSize", "Change Point Size", min=4, max=24, value=8, step=2,width=150),
                      sliderInput("densFontSize", "Change Font Size", min=12, max=32, value=20, step=2,width=150)
                    )),
                  div(class="panel-footer", 
                        actionButton(inputId="CloseDisplayOptions",class="btn btn-primary",label="Close"))
                  ))
                  
                  
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
            wellPanel(class="panel panel-default",
              h4("Data:", class="panel-heading"),
              tags$div(
                title="Choose a park to work with.",
                uiOutput("IVParkControl")
              ),
              br(),
              tags$div(
                title="Choose the type of plant you want to work with", 
                selectizeInput(inputId="IVGroup", label="Type of plant:",choices=IVPlantTypes)
              ),
              br(),
              tags$div(
                title="Toggle between common and scientific names",
                checkboxInput(inputId="IVCommon", label="Show common names?", value=TRUE)
              ),
              br(),
              # tags$div(
              #   title="Pick the four year period you want to graph",
              #   sliderInput(inputId="IVYear", label="Display data from the 4 years ending:", min=Years$Start+Years$Range-1, 
              #               max=Years$End, value=Years$End, sep="", step=1,ticks=T)
              # ),
              tags$div(title="Choose the time period you want to work with.", uiOutput("IVCycleControl")),
              br(),
              tags$div(
                title="Show density, size and disbribution separately",
                checkboxInput(inputId="IVPart", label="Show Components of the Importance Value?", value=FALSE)
              ),
              br(),
              tags$div(
                title="Chose the maximum number of species to display.",
                sliderInput(inputId="IVTop",label="Number of species to plot (in order of IV):",min=1, max=20,
                  value=10, sep="", step=1, ticks=FALSE)),
                conditionalPanel(
                  condition="input.IVPanel=='Graph'",
                  hr(),
                  actionButton(inputId="IVGraphButton", label="Display Options", class="btn btn-primary"),
                  br(),
                  downloadButton(outputId="IVGraphDownload", label="Save Graph (.jpg)", class="btn btn-primary"),
                  downloadButton(outputId="IVWmfDownload", label="Save Graph (.wmf)", class="btn btn-primary")
                ),  
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
                hidden(
                  fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",top=160,bottom="auto",height="auto",
                             left=575,width="auto",id="IVOptionsPanel",style="padding: 0px",title="Display Options",
                    div(class="panel-heading", h4("Display Options")),
                    div(class="panel-body",
                      flowLayout(
                        selectizeInput("IVBaseColor","Base Color:",choices=ColorNames, selected="green4",width="125px"),
                        sliderInput("IVFontSize", "Change Font Size", min=10, max=24, value=14, step=2,width="175px")
                      ),
                      h5("Component Colors:"),
                      flowLayout(
                        selectizeInput("IVDensityColor","Density Color:",choices=ColorNames, selected="green4", width="125px"),
                        selectizeInput("IVSizeColor","Size Color:",choices=ColorNames, selected="chartreuse",width="125px"),
                        selectizeInput("IVDistributionColor","Distribution Color:",choices=ColorNames, selected="yellow",width="125px")
                      )
                    ),    
                    div(class="panel-footer", actionButton(inputId="CloseIVDisplayOptions",class="btn btn-primary",label="Close"))
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
      column(4,id="SpeciesControls",
        tags$head(tags$style(HTML("#SpeciesControls{height:400px}"))),
        wellPanel(
          tags$div(
            title="Choose the type of species list", 
            radioButtons(inputId="SpListType", label="Choose a species list:",
                choices=c("Vascular plants in the monitorng plots"= "Monitoring", "All vascular plants known from the park"="NPSpecies"))
          ),
          tags$div(
            title="Choose a park to work with.",uiOutput("SpListParkControl")
          ),
          conditionalPanel(condition="input.SpListType=='Monitoring'",
            tags$div(
              title="Choose one or more plots, select and backspace to delete.", uiOutput("SpListPlotControl")
            )
          )
        )
      ),
      column(6,
        tabsetPanel(id="SpeciesListPanel", type="pills",
          tabPanel("Species Lists",
            h3(textOutput("SpeciesTableTitle")),
            DT::dataTableOutput("SpeciesTable")
          ),
          tabPanel("About these lists...",
            includeHTML("./www/AboutLists.html")
          )
        )
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
      ProjectInfo
    ),



################ Citations 

    tabPanel("Citations & References",
     Citations
    )
) #end About menu
)#end navbarPage()
