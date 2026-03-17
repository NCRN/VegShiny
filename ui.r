library(shiny)
library(NPSForVeg)
library(leaflet)
library(shinyjs)
library(DT)


shiny::navbarPage(title=htmltools::HTML("<div> <a href=",NETWORKURL,"></a> National Capital Region Network <br> Forest Vegetation </div>"),
    position = "static-top", inverse=TRUE, collapsible = TRUE, fluid=TRUE, windowTitle = base::paste(NETWORK, "Forest Vegetation"),
    theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", id="MainNavBar",
    
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("
    .navbar .navbar-brand,
    .navbar-inverse .navbar-brand {
      font-size: 40px !important; 
      font-family: 'Times New Roman';
      color: #ffffff !important;                    
      line-height: 1.0;
      padding-left: 25px;
    }

    /* Increase navbar height so the two-line title has room */
    .navbar {
      min-height: 130px;}
    }}
  ")),
##### google analytics #####
      htmltools::includeHTML("www/google-analytics.html"),
      
##### map panel - mobile scroll ***not working*** #####    
#      tags$style(HTML("
#    /* Let vertical gestures scroll the page even when touching the map */
#    .leaflet-container {
#      touch-action: pan-y !important;
#      overscroll-behavior: contain;
#    }
# ")),
      ),
    
  ######################################### Map Panel ####################################################################
  
  
   shiny::tabPanel(title="Map",style="padding: 0",
     shinyjs::useShinyjs(),

   htmltools::div(class="outer",
      htmltools::tags$head(shiny::includeCSS("./www/mapstyles.css") ), # defines css file
      htmltools::tags$head(shiny::includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
    ),
    
#### Side Control Panel ####    
    shiny::fluidRow(
      shiny::column(2, style="padding: 0 0 0 10px",
#### Map Controls ####
      htmltools::div(id="MapControlPanel",class="panel panel-default controls",
            shiny::h4("Map Controls", class="panel-heading"),
            htmltools::tags$div(title="Choose the type of plant you want to work with", shiny::selectInput(inputId="MapGroup", 
              label="Type of plant:", choices=PLANTTYPES)),
            htmltools::tags$div(title="Choose live or dead", shiny::selectInput(inputId="TreeStatus", label="Live or dead",
                                                               choices=base::c("Live"='alive',"Dead" = 'snag',"All"='all'))),
            htmltools::tags$div(title="Type of data to map",shiny::uiOutput("PlantValueControl")),
            htmltools::tags$div(title="Choose the time period you want to work with.", shiny::uiOutput("MapCycleControl")),
            #htmltools::tags$div(title="Choose the four year period you want to work with.", shiny::sliderInput(inputId="MapYear", 
             #     label="Display data from the 4 years ending:", min=YEARS$Start+YEARS$Range-1, max=YEARS$End, value=YEARS$End,
              #    sep="", step=1,ticks=T)),
            htmltools::tags$div(title="Toggle between common and scientific names",
                             shiny::checkboxInput(inputId="mapCommon", label="Show common names?", value=TRUE )),
            htmltools::tags$div(title="Choose a species of plants to map", shiny::uiOutput("MapSpeciesControl")),  
            htmltools::tags$div(title="Filter the species list so only species found in a particular park are listed",
                           shiny::uiOutput("MapParkControl")),
            shiny::actionButton(inputId="AboutMapButton",label="About the map",class="btn btn-primary ")
           ),
        
#### Zoom Controls ####
     htmltools::div(id="ZoomPanel",class="panel panel-default controls",
          shiny::h4("Zoom to:", class="panel-heading"),
          shiny::fluidRow(
            shiny::column(9, htmltools::tags$div(title="Choose a park and click 'Go'", shiny::uiOutput("ParkZoomControl"))),
            shiny::column(3, shiny::actionButton(inputId="MapZoom", label="Go", class="btn btn-primary btn-sm"))
          ),
          shiny::hr(),
          htmltools::tags$div(title="Increases size of plots for easier viewing",
           shiny::radioButtons(inputId="PlotSize", label="Enlarge plots: 1X = to scale", 
             choices=base::c("1X"=1, "5X"=base::sqrt(5), "10X"=base::sqrt(10), "25X"=5), selected="1", inline=TRUE)
          )
      ),

#### Add a layer control ####
    
     htmltools::div(id="ExtraLayerPanel",class="panel panel-default controls",draggable=TRUE,cursor="auto",top="60%",bottom="auto",
                          height="auto",right="auto",left=20,width=200,
        shiny::h4("Additional Layers", class="panel-heading"),
        htmltools::tags$div(title="Overlay additional data onto the parks",
           shiny::selectizeInput(inputId="MapLayer", label="Add a map layer:", 
                    choices=EXTRALAYERS))
        )
      ), ## End of controls columns

#### The Map ####
       shiny::column(10,style="padding: 0",
          htmltools::div(leaflet::leafletOutput("VegMap", height="1000px"))   
       ),
# CSS
tags$head(tags$style(HTML("
  /* Desktop default: keep your 1000px if desired */
  #VegMapContainer { height: 1000px; }

  /* On small screens, make map fill available viewport */
  @media (max-width: 768px) {
    #VegMapContainer {
      height: calc(100vh - 120px); /* subtract navbar+some margin */
    }
    #VegMapContainer .leaflet-container {
      height: 100% !important;
    }
  }")))

     ),
# #### Floating "About the map" Panel ####
  shiny::fixedPanel(class="panel panel-primary controls",draggable=TRUE,
             cursor="auto",top=80,bottom="auto",height="520",
             left=450,width="500",id="AboutMapPanel",style="padding: 0px; display:none;",
            htmltools::div(class="panel-heading", shiny::h4("About the Map" )),
            htmltools::div(class="panel-body",style="height: 400px;  overflow-y: scroll",  htmltools::includeHTML("./www/AboutMap.html")),
            htmltools::div(class="panel-footer",
                 shiny::actionButton(inputId="CloseAboutMap",class="btn btn-primary",label="Close"))  )
),  ## end of map page


######################################## Graphs Panel ##########################################################

    shiny::navbarMenu(htmltools::tags$div(title="Graph the data","Graphs"),    
#############  densplot() based plots
      shiny::tabPanel(htmltools::tags$div(title="Graph abundance, basal area, percent cover, etc.","Data by Park and Species"), 
               
               shinyjs::useShinyjs(),
        shiny::fluidRow(
          shiny::column(3,
            shiny::wellPanel(class="panel panel-default",
              shiny::h4("Data:", class="panel-heading"),
              htmltools::tags$div(title="Choose a park to work with.",
                       shiny::uiOutput(outputId="densParkControl")
              ),
              htmltools::tags$div(title="Choose the time period you want to work with.", shiny::uiOutput("densCycleControl")),
              # htmltools::tags$div(title="Pick the four year period you want to graph",
              #          shiny::sliderInput(inputId="densYear", label="Display data from the 4 years ending:", 
              #                      min=YEARS$Start+YEARS$Range-1, max=YEARS$End, value=YEARS$End,
              #                      sep="", step=1,ticks=T)
              # ),
              htmltools::tags$div(title="Choose the type of plant you want to work with", 
                shiny::selectizeInput(inputId="densGroup", label="Type of plant:",   choices=PLANTTYPES)
              ),
              htmltools::tags$div(title="Toggle between common and scientific names",
                       shiny::checkboxInput(inputId="densCommon", label="Show common names?", value=TRUE )
              ),
              htmltools::tags$div(title="Graph the most common species, species you choose, or all species combined.",
                shiny::radioButtons(inputId="densSpeciesType", label="Which species?", 
                 choices=base::c("Most common species"="Common","Pick individual species"="Pick",
                          "All species combined"="All"),inline=FALSE)
              ),
              shiny::uiOutput(outputId="densSpeciesControl"),
              htmltools::tags$div(title="Type of data to graph",
                shiny::uiOutput(outputId="densValControl")
              ),
              shiny::conditionalPanel(
                condition="input.densPanel=='Graph'",
                shiny::actionButton(inputId="densGraphButton", label="Display Options", class="btn btn-primary"),
               htmltools::div(shiny::downloadButton(outputId="densGraphDownload", label="Save Graph (.jpg)", class="btn btn-primary"),
                shiny::downloadButton(outputId="densWmfDownload", label="Save Graph (.png)", class="btn btn-primary"))
              ),
              shiny::conditionalPanel(
                condition="input.densPanel=='Table'",
                shiny::hr(),
                shiny::downloadButton(outputId="densTableDownload", label="Save Table (.csv)", class="btn btn-primary")
              )
            ),
            shiny::conditionalPanel(
              condition="input.densPanel=='Graph'",
              shiny::wellPanel(class="panel panel-default",
                shiny::h4("Comparison Data:", class="panel-heading"),
                  htmltools::tags$div(title="Compare the base data with a differnet park, growth stage, or time period",
                  shiny::radioButtons(inputId="CompareType", label ="Compare to another:",
                           choices=base::c("None","Park","Growth Stage","Time"),selected="None",inline=TRUE)
                  ),
                  shiny::uiOutput(outputId="CompareSelect")
              )
            )
          ),
          shiny::column(9,
            shiny::tabsetPanel(id="densPanel",type="pills",
                shiny::tabPanel(title=htmltools::tags$div(title="Graph the data", "Graph"),value="Graph",
                  htmltools::tags$div(title="Mean and 95% Confidence interval",
                  shiny::plotOutput(outputId="DensPlot", height="600px")),
             
                    shiny::fixedPanel(class="panel panel-primary controls",draggable=TRUE,
                               cursor="auto",top=160,bottom="auto",height="auto",
                               left=575,width="auto",id="GraphOptionsPanel",style="padding: 0px; display: none;",
                      title="Display Options",
                   htmltools::div(class="panel-heading", shiny::h4("Display Options")),
                   htmltools::div(class="panel-body",
                    shiny::flowLayout(cellArgs=base::list(style="width: 160px"),
                      shiny::selectizeInput("densBaseColor","Base Data Color:",choices=COLORNAMES, selected="blue",width=150),
                      shiny::selectizeInput("densCompareColor","Comparison Data Color:",choices=COLORNAMES, selected="red",width=150)
                    ),
                    shiny::br(),
                    shiny::flowLayout(cellArgs=base::list(style="width: 160px"),
                      shiny::sliderInput("densPointSize", "Change Point Size", min=4, max=24, value=8, step=2,width=150),
                      shiny::sliderInput("densFontSize", "Change Font Size", min=12, max=32, value=20, step=2,width=150)
                    )),
                 htmltools::div(class="panel-footer",
                        shiny::actionButton(inputId="CloseDisplayOptions",class="btn btn-primary",label="Close"))
                  )
                  
                  
                ),
              shiny::tabPanel(
                htmltools::tags$div(title="See all data in a table","Data table"),
                value="Table",
                shiny::column(10,
                 shiny::h3(shiny::textOutput("densTableTitle")),
                 shiny::dataTableOutput("densTable")
                )
              ),
              shiny::tabPanel(htmltools::tags$div(title="Explanation of the graph","About this graph..."),
                       htmltools::includeHTML("./www/DensPlot.html")
              )
            )
          )
        )
      ),

###############IV Plots
      shiny::tabPanel(htmltools::tags$div(title="Graph Importance Values", "Forestry Importance Values (IV)"),
        shiny::fluidRow(
          shiny::column(3,
            shiny::wellPanel(class="panel panel-default",
              shiny::h4("Data:", class="panel-heading"),
              htmltools::tags$div(
                title="Choose a park to work with.",
                shiny::uiOutput("IVParkControl")
              ),
              shiny::br(),
              htmltools::tags$div(
                title="Choose the type of plant you want to work with", 
                shiny::selectizeInput(inputId="IVGroup", label="Type of plant:",choices=IVPLANTTYPES)
              ),
              shiny::br(),
              htmltools::tags$div(
                title="Toggle between common and scientific names",
                shiny::checkboxInput(inputId="IVCommon", label="Show common names?", value=TRUE)
              ),
              shiny::br(),
              # htmltools::tags$div(
              #   title="Pick the four year period you want to graph",
              #   shiny::sliderInput(inputId="IVYear", label="Display data from the 4 years ending:", min=YEARS$Start+YEARS$Range-1, 
              #               max=YEARS$End, value=YEARS$End, sep="", step=1,ticks=T)
              # ),
              htmltools::tags$div(title="Choose the time period you want to work with.", shiny::uiOutput("IVCycleControl")),
              shiny::br(),
              htmltools::tags$div(
                title="Show density, size and disbribution separately",
                shiny::checkboxInput(inputId="IVPart", label="Show Components of the Importance Value?", value=FALSE)
              ),
              shiny::br(),
              htmltools::tags$div(
                title="Chose the maximum number of species to display.",
                shiny::sliderInput(inputId="IVTop",label="Number of species to plot (in order of IV):",min=1, max=20,
                  value=10, sep="", step=1, ticks=FALSE)),
                shiny::conditionalPanel(
                  condition="input.IVPanel=='Graph'",
                  shiny::hr(),
                  shiny::actionButton(inputId="IVGraphButton", label="Display Options", class="btn btn-primary"),
                  shiny::br(),
                  shiny::downloadButton(outputId="IVGraphDownload", label="Save Graph (.jpg)", class="btn btn-primary"),
                  shiny::downloadButton(outputId="IVWmfDownload", label="Save Graph (.wmf)", class="btn btn-primary")
                ),  
                  shiny::conditionalPanel(
                    condition="input.IVPanel=='Table'",
                    shiny::hr(),
                    shiny::flowLayout(
                      shiny::downloadButton(outputId="IVTableDownload", label="Save Table (.csv)", class="btn btn-primary")
                    )
                )
            )
          ),
          shiny::column(9,
            shiny::tabsetPanel(id="IVPanel",type="pills",
              shiny::tabPanel(value="Graph",
                htmltools::tags$div(title="Graph the data","Graph"),
                htmltools::tags$div(title="Graph of IV",shiny::plotOutput("IVPlot",height="600px")),
              
                  shiny::fixedPanel(class="panel panel-primary controls",draggable=TRUE,
                             cursor="auto",top=160,bottom="auto",height="auto",
                             left=575,width="auto",id="IVOptionsPanel",style="padding: 0px; display: none;",title="Display Options",
                   htmltools::div(class="panel-heading", shiny::h4("Display Options")),
                   htmltools::div(class="panel-body",
                      shiny::flowLayout(
                        shiny::selectizeInput("IVBaseColor","Base Color:",choices=COLORNAMES, selected="green4",width="125px"),
                        shiny::sliderInput("IVFontSize", "Change Font Size", min=10, max=24, value=14, step=2,width="175px")
                      ),
                      shiny::h5("Component Colors:"),
                      shiny::flowLayout(
                        shiny::selectizeInput("IVDensityColor","Density Color:",choices=COLORNAMES, selected="green4", width="125px"),
                        shiny::selectizeInput("IVSizeColor","Size Color:",choices=COLORNAMES, selected="chartreuse",width="125px"),
                        shiny::selectizeInput("IVDistributionColor","Distribution Color:",choices=COLORNAMES, selected="yellow",width="125px")
                      )
                    ),
                   htmltools::div(class="panel-footer", shiny::actionButton(inputId="CloseIVDisplayOptions",class="btn btn-primary",label="Close"))
                  )
              ),
              shiny::tabPanel(value="Table",
                htmltools::tags$div(title="See all data in a table","Data table"),
                shiny::column(10,
                  shiny::h3(shiny::textOutput("IVTableTitle")),
                 shiny::dataTableOutput("IVData")
                )
              ),
              shiny::tabPanel(htmltools::tags$div(title="Explanation of the graph",
                  "About this graph..."
                ),
                htmltools::includeHTML("./www/IVPlot.html")
              )
            )
          )
        )
      )
    ),

############################## Species Lists
    shiny::tabPanel(id="SpeciesPanel",
      htmltools::tags$div(
        title="Lists of plants found in the parks", "Species Lists"
      ),
      shiny::column(4,id="SpeciesControls",
        htmltools::tags$head(htmltools::tags$style(htmltools::HTML("#SpeciesControls{height:400px}"))),
        shiny::wellPanel(
          htmltools::tags$div(
            title="Choose the type of species list", 
            shiny::radioButtons(inputId="SpListType", label="Choose a species list:",
                 choices=base::c("Vascular plants in the monitorng plots"= "Monitoring", "All vascular plants known from the park"="NPSpecies"))
          ),
          htmltools::tags$div(
            title="Choose a park to work with.",shiny::uiOutput("SpListParkControl")
          ),
          shiny::conditionalPanel(condition="input.SpListType=='Monitoring'",
            htmltools::tags$div(
              title="Choose one or more plots, select and backspace to delete.", shiny::uiOutput("SpListPlotControl")
            )
          )
        )
      ),
      shiny::column(6,
        shiny::tabsetPanel(id="SpeciesListPanel", type="pills",
          shiny::tabPanel("Species Lists",
            shiny::h3(shiny::textOutput("SpeciesTableTitle")),
            DT::dataTableOutput("SpeciesTable")
          ),
          shiny::tabPanel("About these lists...",
            htmltools::includeHTML("./www/AboutLists.html")
          )
        )
      )
    ),

##################### About
    shiny::navbarMenu(
      htmltools::tags$div(
        title="About the project", "About"
        ),

################################# Project Information
    shiny::tabPanel(
      htmltools::tags$div(
        title="Background Informaiton", "Project Information"
      ),
      PROJECTINFO
    ),



################ Citations 

    shiny::tabPanel("Citations & References",
     CITATIONS
    )
) #end About menu
)#end shiny::navbarPage()
