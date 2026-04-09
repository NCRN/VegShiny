library(shiny)
library(NPSForVeg)
library(leaflet)
library(shinyjs)
library(DT)
library(bslib)
library(htmltools)


shiny::navbarPage(
  title=htmltools::HTML("National&nbsp;Capital&nbsp;Region&nbsp;Network<wbr> Forest&nbsp;Vegetation"),
  position = "static-top", inverse=TRUE, collapsible = TRUE, fluid=TRUE, windowTitle = base::paste(NETWORK, "Forest Vegetation"),
  theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", id="MainNavBar",
                  
  htmltools::tags$head(shiny::includeCSS("./www/mapstyles.css"), # defines css file
    shiny::includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"),
    htmltools::tags$style(
      htmltools::HTML("
      .navbar-inverse .navbar-brand {
        font-size: calc(1.5rem + 1.5vw);      
        font-family: 'Times New Roman';
        color: #fff !important;                    
        line-height: 1.0;
        padding-left: 1rem;
        margin: 0;
        pointer-events:none;}

      .navbar {
        z-index: 2000;
        min-height: auto !important;            
        padding-top: 1.5rem;                    
        padding-bottom: 1.5rem;}
        
      .navbar-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        width: 100%;
        float: none;             
        gap: clamp(0.5rem, 1vw, 1rem);}

      .navbar-header .navbar-brand {
        order: 1;                     
        text-align: left;           
        margin: 0;            
        display: flex;
        align-items: center;}

      .navbar-header .navbar-toggle {
        order: 2;                     
        align-items: center;
        margin-left: auto;            
        margin-right: clamp(0.5rem, 2vw, 2rem);  
        display: inline-flex !important;
        flex-direction: column;           
        justify-content: center;          
        margin-top: 0 !important;         
        margin-bottom: 0 !important;      
        padding: 0 !important;}

      .navbar-collapse.collapse {display: none !important;}
      .navbar-collapse.in {display: block !important;}

      .navbar-nav {
        display: flex;   
        float: none !important; 
        align-items: left;
        gap: 0.75rem;                  
        flex-direction: column;
        margin: 0;}

      .navbar-toggle {border: none !important;}
      .navbar-toggle:hover,
      .navbar-toggle:active,
      .navbar-toggle:focus {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        outline: none !important;}

      .navbar-toggle > span:not(.icon-bar) { display: none !important; }

      .navbar-toggle::before,
      .navbar-toggle::after { content: none !important; display: none !important; }

      .navbar-toggle .icon-bar {
        display: block !important;
        width: 1.6em;
        height: 0.10em;
        background-color: #fff !important;}
      .navbar-toggle .icon-bar + .icon-bar { margin-top: 0.4em; }

      .navbar-nav .dropdown-menu {
        position: static !important;   
        float: none !important;
        margin-top: 0 !important;
        display: none;                 
        border: none;                  
        box-shadow: none;             
        background: #000;              
        margin-left: 1.5rem;}
        
      .navbar-nav .dropdown.open > .dropdown-menu {display: block;}

      .navbar-nav > li > a,
      .navbar-nav .dropdown-menu > li > a{
        font-family: 'Times New Roman' !important;
        font-size: calc(.75em + .75vw) !important;
        color: #fff !important;}

      .navbar-nav > li > a:empty {
        display: none !important;
        visibility: hidden !important;
        height: 0 !important;
        padding: 0 !important;
        margin: 0 !important;}

      .navbar.navbar-inverse .navbar-nav li.dropdown.open a.dropdown-toggle .caret {
        transform: scaleY(-1) translateX(-2px);
        align-self: center !important;
        margin-left: 0.35rem;}
      .navbar.navbar-inverse .navbar-nav > li.dropdown > a.dropdown-toggle {
        display: flex !important;
        align-items: center !important;
        width: 100% !important;
        padding: 0.75rem 1.25rem !important;}

      .navbar-inverse .navbar-nav > li > a {padding: 0.75rem 1.25rem;}
      .navbar-inverse .navbar-nav > li > a:hover,
      .navbar-nav .dropdown-menu > li > a:hover {
        color: #fff !important;
        background-color: #916800 !important;}

      .navbar-nav .dropdown-menu > li > a:focus,
      .navbar-nav .dropdown-menu > li > a:active {
        outline: 0 !important;}
      .navbar-nav .dropdown-menu > .active > a,
      .navbar-nav .dropdown-menu > .active > a:hover,
      .navbar-nav .dropdown-menu > .active > a:focus {
        background-color: transparent;
        color: #fff !important;}

      .selectize-control .selectize-input::after {
        margin-top: 0 !important;
        margin-right: 0 !important;}

      .selectize-control .selectize-input {
        position: relative;}
      .selectize-control .selectize-input::after {
        position: absolute;
        right: 1rem !important;
        transform: translateY(-50%);}
        
      @media (max-width: 630px) {
        #AboutMapPanel {
          top: 85px;
          right: 30px;
          width: 250px !important;
          height: 500px !important}
        #AboutMapPanel .panel-body {height: 375px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 85px;
          right: 3px;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 225px !important; overflow-y: auto;}
        
        
        
        
        #IVOptionsPanel{
          top: 85px;
          right: 15px;
          width: 240px !important;
          height: 575px !important}
        #IVOptionsPanel .panel-body {height: 450px !important; overflow-y: auto;}}

     
     
     
     
     
      @media (min-width: 631px) and (max-width: 767px) {
        #AboutMapPanel {
          top: 85px;
          right: 30px;
          width: 300px !important;
          height: 500px !important;}
        #AboutMapPanel .panel-body {height: 375px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 85px;
          right: 30px;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 225px !important; overflow-y: auto;}
        #IVOptionsPanel{
          top: 85px;
          right: 30px;
          width: 500px !important;
          height: 450px !important}
        #IVOptionsPanel .panel-body {height: 325px !important; overflow-y: auto;}}
        
      @media (min-width: 768px) and (max-width: 1000px) {
        #AboutMapPanel {
          top: 85px;
          left: 25vw;
          width: 450px !important;
          height: 675px !important;}
        #AboutMapPanel .panel-body {height: 550px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 150px;
          left: 27vw;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 225px !important; overflow-y: auto;}
        #IVOptionsPanel{
          top: 150px;
          left: 27vw;
          width: 550px !important;
          height: 450px !important}
        #IVOptionsPanel .panel-body {height: 325px !important; overflow-y: auto;}}
      
      @media (min-width: 1001px) and (max-width: 1597px) {
        #AboutMapPanel {
          top: 85px;
          left: 22vw;
          width: 450px !important;
          height: 450px !important;}
        #AboutMapPanel .panel-body {height: 327px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 150px;
          left: 26.25vw;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 227px !important; overflow-y: auto;}
        #IVOptionsPanel{
          top: 140px;
          left: 26.25vw;
          width: 725px !important;
          height: 345px !important}
        #IVOptionsPanel .panel-body {height: 223px !important; overflow-y: none;}}
      
      @media (min-width: 1598px) {
        #AboutMapPanel {
          top: 85px;
          left: 20vw;
          width: 600px !important;
          height: 825px !important;}
        #AboutMapPanel .panel-body {height: 700px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 150px;
          left: 25.75vw;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 225px !important; overflow-y: auto;}
        #IVOptionsPanel{
          top: 150px;
          left: 25.75vw;
          width: 600px !important;
          height: 450px !important}
        #IVOptionsPanel .panel-body {height: 325px !important; overflow-y: auto;}}
  ")),
    
    htmltools::tags$script(htmltools::HTML("
    $(document).on('click', '.navbar-collapse.in a:not(.dropdown-toggle)', function () {
      $(this).closest('.navbar-collapse').collapse('hide');
    });

    $(document).on('click', '.navbar-collapse .dropdown-menu a', function () {
      $(this).closest('.navbar-collapse').collapse('hide');
    });
    
    $(function(){
      $('.navbar-nav li.dropdown').each(function(){
        var $a = $(this).find('> a.dropdown-toggle');
        var $t = $a.find('[title]');
        var title = $t.attr('title');
        if (title) {
          $(this).attr('title', title); 
          $t.attr('title','');}});
    });

    (function () {function getSelectize(el) { return el && el.selectize ? el.selectize : null; }

    $(document).on('shiny:bound', function (ev) {
      var $el = $(ev.target);
      if (!$el.is('select') || !$el.hasClass('selectized')) return;
      var sel = getSelectize($el[0]);
      if (!sel) return;
      sel.settings.openOnFocus = false;
      var $control = $el.next('.selectize-control').find('.selectize-input');

  //close menu upon selecting toggle
    $control.on('pointerdown.selectizeToggle', function (e) {
      if ($(e.target).is('input, textarea')) return;
      e.preventDefault();
      e.stopImmediatePropagation();
      if (sel.isOpen) {sel.close(); sel.blur();} 
      else {sel.open();sel.focus();}
    });

  //close menu upon item selection
    sel.on('item_select', function () { sel.close(); sel.blur(); });
    sel.on('dropdown_close', function () { sel.blur(); });
    });

  //close menu when clicking anywhere outside menu 
    $(document).on('pointerdown.selectizeOutside', function (e) {
      var $t = $(e.target);
      if ($t.closest('.selectize-control').length) return;
    $('.selectized').each(function () {
      var sel = getSelectize(this);
      if (sel && sel.isOpen) sel.close();});
    });
    })();
  ")),
    
    htmltools::includeHTML("www/google-analytics.html")
),
  
  ######################################### Map Panel ####################################################################
  
   shiny::tabPanel(htmltools::tags$div(title="Map the data", "Map"), style="padding: 0",
     shinyjs::useShinyjs(),
    
#### Side Control Panel ####    
    shiny::fluidRow(
      shiny::column(2, class = "sidebar-col", style="padding: 0 5px 0 5px",
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
            shiny::actionButton(inputId="AboutMapButton",label="About the map",class="btn btn-primary")
           ),
        
#### Zoom Controls ####
     htmltools::div(id="ZoomPanel",class="panel panel-default controls",
          shiny::h4("Zoom to:", class="panel-heading"),
          shiny::fluidRow(
            shiny::column(9, htmltools::tags$div(title="Choose a park and click 'Go'", shiny::uiOutput("ParkZoomControl"))),
            shiny::column(3, shiny::actionButton(inputId="MapZoom", label="Go", class="btn btn-primary btn-block action-button"))
          ),
          shiny::hr(),
          htmltools::tags$div(title="Increases size of plots for easier viewing",
           shiny::radioButtons(inputId="PlotSize", label="Enlarge plots: 1X = to scale", 
             choices=base::c("1X"=1, "5X"=base::sqrt(5), "10X"=base::sqrt(10), "25X"=5), selected="1", inline=TRUE)
          ),
          htmltools::tags$style(
            htmltools::HTML("@media (min-width: 678px) and (max-width: 1597px) {
                                #ZoomPanel .row > [class*='col-'] {
                                  float: none !important;
                                  width: 100% !important;}
                                #ZoomPanel .btn.action-button {
                                  width: 100% !important;}}
                              @media (min-width: 1598px) {                
                                #ZoomPanel .col-sm-3 {
                                  padding-left: 6px !important;
                                  padding-right: 15px !important;}
                                #ZoomPanel .col-sm-9 { 
                                  padding-left: 15px !important; 
                                  padding-right: 6px !important; }}
                                .action-button {white-space: normal !important;}"))
      ),

#### Add a layer control ####
    
     htmltools::div(id="ExtraLayerPanel",class="panel panel-default controls",draggable=TRUE,cursor="auto",top="60%",bottom="auto",
                          height="auto",right="auto",left=20,width=200,
        shiny::h4("Additional Layers", class="panel-heading", style="
      overflow: visible !important;  /* avoid clipping */
      overflow-wrap: break-word;     /* modern word breaking */"),
        htmltools::tags$div(title="Overlay additional data onto the parks",
           shiny::selectizeInput(inputId="MapLayer", label="Add a map layer:", 
                    choices=EXTRALAYERS))
        )
      ), ## End of controls columns

#### The Map ####
       shiny::column(10,style="padding: 0 0 0 0px",
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
             cursor="auto", id="AboutMapPanel",style="padding: 0px; display:none; z-index: 1995;",
            htmltools::div(class="panel-heading", shiny::h4("About the Map" )),
            htmltools::div(class="panel-body",  htmltools::includeHTML("www/AboutMap.html")),
            htmltools::div(class="panel-footer",
                 shiny::actionButton(inputId="CloseAboutMap",class="btn btn-primary",label="Close")))
),  ## end of map page


######################################## Graphs Panel ##########################################################

    shiny::navbarMenu(htmltools::tags$div(title="Graph the data", "Graphs"),
                      
#############  densplot() based plots
      shiny::tabPanel(htmltools::tags$div(title="Graph abundance, basal area, percent cover, etc.","Data by Park and Species"),
               shinyjs::useShinyjs(),
        shiny::fluidRow(
          shiny::column(3,
                        htmltools::tags$head(htmltools::tags$style(
                          htmltools::HTML(".action-button {
                                              white-space: normal !important;
                                              word-wrap: break-word !important;}"))),
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
                          "All species combined"="All"), inline=FALSE)
              ),
              shiny::uiOutput(outputId="densSpeciesControl"),
              htmltools::tags$div(title="Type of data to graph",
                shiny::uiOutput(outputId="densValControl")
              ),
              shiny::conditionalPanel(
                condition="input.densPanel=='Graph'",
                shiny::actionButton(inputId="densGraphButton", label="Display Options", class="btn btn-primary btn-block action-button"),
              #  br(),
              # htmltools::div(shiny::downloadButton(outputId="densGraphDownload", label="Save Graph (.jpg)", class="btn btn-primary btn-block action-button"),
              #  shiny::downloadButton(outputId="densWmfDownload", label="Save Graph (.png)", class="btn btn-primary btn-block action-button"))
              ),
              shiny::conditionalPanel(
                condition="input.densPanel=='Table'",
                shiny::hr(),
                shiny::downloadButton(outputId="densTableDownload", label="Save Table (.csv)", class="btn btn-primary btn-block action-button")
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
                               cursor="auto",id="GraphOptionsPanel",style="padding: 0px; display: none; z-index: 1995;",
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
                shiny::column(10, style="padding: 5px",
                 shiny::h3(shiny::textOutput("densTableTitle")),
                 DT::dataTableOutput("densTable")
                )
              ),
              shiny::tabPanel(htmltools::tags$div(title="Explanation of the graph","About this graph..."),
                       htmltools::includeHTML("www/DensPlot.html")
              )
            )
          )
        )
      ),

###############IV Plots
      shiny::tabPanel(htmltools::tags$div(title="Graph Importance Values", "Forestry Importance Values (IV)"),
        shiny::fluidRow(
          shiny::column(3,
                        htmltools::tags$head(htmltools::tags$style(
                          htmltools::HTML(".action-button {
                                              white-space: normal !important;
                                              word-wrap: break-word !important;}"))),
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
                  shiny::actionButton(inputId="IVGraphButton", label="Display Options", class="btn btn-primary btn-block action-button"),
                  shiny::br(),
                  shiny::downloadButton(outputId="IVGraphDownload", label="Save Graph (.jpg)", class="btn btn-primary btn-block action-button"),
                  shiny::downloadButton(outputId="IVWmfDownload", label="Save Graph (.wmf)", class="btn btn-primary btn-block action-button")
                ),  
                  shiny::conditionalPanel(
                    condition="input.IVPanel=='Table'",
                    shiny::hr(),
                    shiny::downloadButton(outputId="IVTableDownload", label="Save Table (.csv)", class="btn btn-primary btn-block action-button")
                    
                )
            )
          ),
          shiny::column(9,
            shiny::tabsetPanel(id="IVPanel",type="pills",
              shiny::tabPanel(value="Graph",
                htmltools::tags$div(title="Graph the data","Graph"),
                htmltools::tags$div(title="Graph of IV",shiny::plotOutput("IVPlot",height="600px")),
              
                  shiny::fixedPanel(class="panel panel-primary controls",draggable=TRUE,
                             cursor="auto",id="IVOptionsPanel",style="padding: 0px; display: none; z-index: 1995;",title="Display Options",
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
                 DT::dataTableOutput("IVData")
                )
              ),
              shiny::tabPanel(htmltools::tags$div(title="Explanation of the graph",
                  "About this graph..."
                ),
                htmltools::includeHTML("www/IVPlot.html")
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
            htmltools::includeHTML("www/AboutLists.html")
          )
        )
      )
    ),

##################### About
    tabPanel(
      tags$div(
        title="About the project", "About"), 
      includeHTML("www/AboutTab.html") 
      )#end About menu
)#end shiny::navbarPage()
