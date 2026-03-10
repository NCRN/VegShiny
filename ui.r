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
                                                              choices=c("Live"='alive',"Dead" = 'snag',"All"='all'))),
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
  fixedPanel(class="panel panel-primary controls",draggable=TRUE,
             cursor="auto",top=80,bottom="auto",height="520",
             left=450,width="500",id="AboutMapPanel",style="padding: 0px; display:none;",
             div(class="panel-heading", h4("About the Map" )),
             div(class="panel-body",style="height: 400px;  overflow-y: scroll",  includeHTML("./www/AboutMap.html")),
             div(class="panel-footer",
                 actionButton(inputId="CloseAboutMap",class="btn btn-primary",label="Close"))  )
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
             
                    fixedPanel(class="panel panel-primary controls",draggable=TRUE,
                               cursor="auto",top=160,bottom="auto",height="auto",
                               left=575,width="auto",id="GraphOptionsPanel",style="padding: 0px; display: none;",
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
              
                  fixedPanel(class="panel panel-primary controls",draggable=TRUE,
                             cursor="auto",top=160,bottom="auto",height="auto",
                             left=575,width="auto",id="IVOptionsPanel",style="padding: 0px; display: none;",title="Display Options",
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
################## About
tabPanel(
  tags$div(
    title="About the project", "About"
    ), 

################## Sections
tags$div(
  style = "margin: 10px 40px;", #indent
    br(),
    tags$p("Updated 2026-03-10"), #update date
    tags$hr(style = "border-top: 1.5px solid #000;"), #separating line
  
  ###Disclaimer
  
    tags$h3("Disclaimer"), #Disclaimer heading
    
    #html block with links
    HTML("
    <p>
      This visualizer tool allows users to explore and analyze vegetation monitoring data.
      It uses data published on the National Park Service (NPS) platform known as the IRMA Data Store:<br>
    <a href='https://irma.nps.gov/DataStore/Reference/2317420'>
      DataStore - National Capital Region Network Forest Vegetation Data Package - cumulative through 2025
    </a>
    </p>
    
    <p>
      This visualizer tool operates using R Shiny code. A static copy of the application source code is available on NPS IRMA Data Store:<br>
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/2317421'>
      DataStore - National Capital Region Network Forest Vegetation Visualizer (R Shiny application)
    </a>
    </p>

    <p>
      The development version of the source code is available at:<br>
    <a href='https://github.com/NCRN/VegShiny/tree/ncrn'>
      https://github.com/NCRN/VegShiny/tree/ncrn
    </a>
    </p>
  "),
    tags$hr(style = "border-top: 1.5px solid #000;"), #bottom separating line

  ###Introduction
  
    tags$h3("Introduction"), #Introduction heading
    
    #html block with links
    HTML("
    <p>
    <a href='https://www.nps.gov/aboutus/index.htm#:~:text=Follow%20Us-,Our%20Mission,of%20this%20and%20future%20generations.'>
      The National Park Service preserves unimpaired the natural and cultural resources and values of the National Park System.
    </a>
    The National Captial Region Inventory and Monitoring Network (NCRN) is part of the 
    <a href='https://www.nps.gov/im/index.htm'>
    National Park Service Inventory and Monitoring Division,
    </a>
    which conducts science in National Parks. 
    <a href='https://www.nps.gov/im/ncrn/index.htm'>
    NCRN conducts science, like water quality monitoring, in the National Park Service's National Capital Region,
    </a> 
    which includes parts of Virginia, West Virginia, Maryland, and the District of Columbia, USA.
    </p>
  "),
    tags$hr(style = "border-top: 1.5px solid #000;"), #bottom separating line
  
  ###Methods
  
    tags$h3("Methods"), #Methods heading
  
    #html block with links
    HTML("
    <p>
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/2210263'>
    Click here to view or download NCRN's forest vegetation monitoring methods
    </a>
    </p>
    
    <h4> Sampling location </h4> 

    <p>
    As of January 2026, NCRN monitors forest vegetation at 432 plots in eleven National Parks in the National Capital Region. 
    NCRN has added and removed plots since 2006. 
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/2210263'>
    Page 5 in NCRN's Long-Term Forest Vegetation Monitoring Protocol details changes to sampling sites through time.
    </a>
    </p>
    <p>
    Each NCRN vegetation monitoring plot meets the following criteria:
    </p>
    <p>
      <div style='margin-left: 20px;'>
      1. Located on park owned land that falls on a point intersection of a 250-meter square grid covering network parks <br>
      2. Randomly selected using a survey design that generates spatially balanced plots (GRTS) <br>
      3. Not stratified based on vegetation or landscape features <br>
      4. Not stratified by park (there is no predetermined number of sites in each park) <br>
      5. Approved by park <br>
      6. Contains forest vegetation <br>
      7. Located on a slope of less than 30° <br>
      8. Safe from hazards and accessible <br>
      9. Free of concerns regarding archeological or cultural resource disturbance <br>
      10. Does not interfere with visitor traffic or other operational concerns <br>
      </div>
    
    <h4> Sampling frequency </h4>
    <p>
    NCRN began monitoring forest vegetation in 2006 and monitoring is ongoing. NCRN has changed sampling frequency since then. 
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/2210263'>
    Pages 7 and 23 in NCRN's Long-Term Forest Vegetation Monitoring Protocol details changes to sampling frequency.
    </a>
    </p>
    <p>
    <ul>
    <li> Since March 2014, location and number of sampling sites have included new plots in ANTI, MONO, and WOTR, bringing the total to 432 active plots </li>
    <li> From May 2009 to 2010, after the first sampling cycle (2006-2009), 
    additional sample plots were added in the smaller NRCN parks, bringing the total to 425 active plots </li>
    <li> From January 2006 to 2008, NCRN monitored 400 plots, divided into four panels of 100 plots, with each panel sampled once every four years </li>
    </ul>
   
    <h4> Parameters </h4>
    <p>
    As of January 2026, NCRN monitors 6 primary vegetation types. A more detailed description of monitored vegetation is availble in
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/2210263'>
    the NCRN forest vegetation monitoring protocol.
    </a>
    Over time, NCRN has added and removed parameters from its monitoring protocol. 
    
      <div style='margin-left: 20px;'>
      1. Trees
      <p>
      Trees are defined as woody plants that have a diameter at breast height (DBH) - diameter taken at a height of 1.37 meters or 4.5 feet - of at least 10 centimeters (~4 inches). 
      Trees are identified to species, their DBH is measured, and plant health assessments are conducted, including: 
      postion in the canopy; presence of injuries to the bark, branches, or large numbers of leaves; 
      presence of diseases or insect pests; and the presence of vines growing on the trees. DBH is used to calculate the 'basal area' of the tree (a cross section of the trunk), 
      which is used as a measure of the size of the tree.
      </p>

      2. Saplings
      <p>
      Saplings are small trees with a DBH between 1 and 10 centimeters (0.4 - 4 inches). Like trees, saplings are identified and their diameter is measured. 
      In addition to the plant health observations for trees, NCRN assess saplings to see if their leaves have been browsed by deer.
      </p>

      3. Shrubs
      <p>
      Shrubs are woody plant species that typically divide into multiple stems at the base of the plants where they emerge from the ground. 
      This differentiates them from trees, which are typically a single stem at the base. Shrubs are identified to species and assessed for evidence of deer browse.
      </p>
    
      4. Tree and Shrub Seedlings </h5>
      <p>
      Seedlings are trees or shrubs with a DBH of less than 1 centimeter. Data is only collected on seedlings that are at least 15 centimeters (~ 6 inches) tall. 
      Smaller seedlings are not recorded. NCRN identifies seedlings to species, measures their height, and looks for evidence of deer browse.
      </p>

      5. Understory Plants
      <p>
      Understory plants is a broad category which inlcudes a variety of species that are found on the forest floor. Only a subset of understory plant species are monitored, 
      primarily species which are either exotic invasives or are known to be preferentailly consumed by deer. Most are not woody, but a few are woody shrubs that 
      either grow as numerous small individuals (e.g. blueberries) or are found in dense thorny thickets (e.g. Japanese barberry). 
      <a href='https://irma.nps.gov/DataStore/Reference/Profile/2210263'>
      The NCRN forest vegetation monitoring protocol***
      </a>
      has a list of all species monitored as understory plants. Note that a plant can be monitored both as an understory plant and as a vine when it occurs both on 
      trees and on the forest floor (e.g. English ivy). NCRN idenitfies each plant to species and estimates its percent cover (this is done by examining a number of 1m2 
      quadrats and determining the percent of the ground covered by that species).
      </p>

      6. Vines in Trees
      <p>
      Every tree in a monitoring plot is inspected for the presence of vines. The species of each vine is recorded. This website provides information on how many trees are hosts to vines. 
      The abundance of each vine species is estimated by the number of trees they grow on and we do not attempt to measure the size of the individual vines.
      </p>
      </div>
    
    <h4> Data Quality </h4>
      <div style='margin-left: 20px;'>
      <h5> 1. Completeness </h5>
      <h5> 2. Accuracy </h5>
      <h5> 3. Validity </h5>
      </div>
    "),
    tags$hr(style = "border-top: 1.5px solid #000;"), #bottom separating line
  
  ###Bibliography
  
    tags$h3("Bibliography"), #Bibliography heading
    
  #html block with links
    HTML("
    <h4> References </h4>
    <p>
    Curtis, JT and RP Macintosh. 1951. 
    <a href='https://doi.org/10.2307/1931725'>
    An upland forest continuum in the prairie-forest border region of Wisconsin.
    </a>
    Ecology 32: 476-496.
    </p>
    <p>
    Jin, S, L Yang, P Danielson, C Homer, J Fry and G Xian. 2013. 
    <a href='https://doi.org/10.1016/j.rse.2013.01.012'>
    A Comprehensive change detection method for updating the National Land Cover Database to circa 2011. 
    </a>
    </p>
    <p>
    Remote Sensing of the Environment 132: 159-175.
    Omernik, J. M. 1995. Ecoregions - a framework for environmental management, in Davis, WS and TP Simon, eds. Biological assessment and criteria-tools for water resource planning and decision making: Boca Raton, Florida, Lewis Publishers, p. 49-62. 
    <a href='https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states'>
    See also - EPA website: Level III and IV Ecoregions of the Continental United States.
    </a>
    </p>
    
    <h4> NCRN Forest Vegetation Publications </h4>
    <p>
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/649487'>
    Schmit JP and Campbell JP. 2007. National Capital Region Network 2006 Forest Vegetation Monitoring Report. Natural Resource Technical Report. NPS/NCRN/NRTR—2007/046. National Park Service. Fort Collins, Colorado
    </a>
    </p>
    <p>
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/660114'>
    Schmit JP and Campbell JP. 2008. National Capital Region Network 2007 Forest Vegetation Monitoring Report. Natural Resource Technical Report. NPS/NCRN/NRTR—2008/125. National Park Service. Fort Collins, Colorado
    </a>
    </p>
    <p>
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/661219'>
    Schmit JP, Campbell P, Parrish J. 2009. National Capital Region Network 2008 Forest Vegetation Monitoring Report. Natural Resource Technical Report. NPS/NCRN/NRTR—2009/181. National Park Service. Fort Collins, Colorado
    </a>
    </p>
    <p>
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/664410'>
    Schmit JP, Campbell JP, Parrish J. 2010. National Capital Region Network 2009 Forest Vegetation Monitoring Report. Natural Resource Data Series. NPS/NCRN/NRDS—2010/043. National Park Service. Fort Collins, Colorado
    </a>
    </p>
    <p>
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/2184360'>
    Schmit JP, Campbell JP, Parrish J. 2012. National Capital Region Network: 2006-2009 forest vegetation status report. Natural Resource Technical Report. NPS/NCRN/NRTR—2012/570. National Park Service. Fort Collins, Colorado
    </a>
    </p>
    <p>
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/2191699'>
    Schmit J, Campbell J, Parrish J. 2012. National Capital Region Network: 2006-2009 forest pest, pathogen and exotic plant status report.. Natural Resource Technical Report. NPS/NCRN/NRTR—2012/650. National Park Service. Fort Collins, Colorado
    </a>
    </p>
    <p>
    <a href='https://irma.nps.gov/DataStore/Reference/Profile/2210263'>
    Schmit JP, Sanders GM, Lehman M, Paradis T, Matthews E. 2014. National Capital Region Network Long-Term Forest Vegetation Monitoring Protocol: Version 2.1 (March, 2014). Natural Resource Report. NPS/NCRN/NRR—2009/113. National Park Service. Fort Collins, Colorado
    </a>
    </p>
    <p>
    <a href='https://doi.org/10.36967/2296913'>
    Schmit JP, Matthews E, Brolis A. 2023. Trends in woody forest vegetation in Prince William Forest Park, 2006–2017. Natural Resource Report. NPS/NCRN/NRR—2023/2495. National Park Service. Fort Collins, Colorado
    </a>
    </p> 
    "),
    tags$hr(style = "border-top: 1.5px solid #000;") #bottom separating line
  
    
)#end of tags$div()
)#end About menu tabPanel()
)#end of navbarPage