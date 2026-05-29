library(shiny)
library(NPSForVeg)
library(leaflet)
library(lattice)
# library(rgdal)
library(shinyjs)
library(jsonlite,pos=100)
library(httr)
library(dplyr)
library(plotly)
library(DT)
library(sf)
library(ritis)
library(tidyr)
library(htmlwidgets)
source('secrets.R')
source('preprocess.r')
preprocess(NETWORK=NETWORK)

### .csv Import ###
VEGDATA<-base::switch(NETWORK,
                      ERMN=importERMN("./Data/ERMN"),
                      MIDN=importMIDN("./Data/MIDN"),
                      NCRN=importNCRN("./Data/NCRN"),
                      NETN=importNETN("./Data/NETN"),
                      SHEN=base::list(importSHEN("./Data/SHEN"))
)

base::names(VEGDATA)<-NPSForVeg::getNames(VEGDATA, name.class="code")
PARKLIST<-NPSForVeg::getNames(VEGDATA,name.class="code")
base::names(PARKLIST)<-NPSForVeg::getNames(VEGDATA)

PARKBOUNDS<-utils::read.csv("boundboxes.csv", as.is=TRUE)

DATACYCLES<-NPSForVeg::getCycles(VEGDATA[[1]])

fmt_common <- function(x) {base::ifelse(base::is.na(x) | !base::nzchar(base::trimws(x)),
    x,
    paste0(base::toupper(base::substr(base::trimws(x), 1, 1)),
           base::tolower(base::substr(base::trimws(x), 2, base::nchar(base::trimws(x))))))}

##### Begin Server Function ####

shiny::shinyServer(function(input,output,session){
  
  #### toggles ####
  shiny::observe({
    ### Maps  
    shinyjs::onclick(id="AboutMapButton", expr= shinyjs::toggle(id="AboutMapPanel"))
    shinyjs::onclick(id="CloseAboutMap", expr= shinyjs::toggle(id="AboutMapPanel")) 
    shinyjs::onclick(id="VideoButton", expr= shinyjs::toggle(id="VideoPanel"))
    shinyjs::onclick(id="CloseVideo", expr= shinyjs::toggle(id="VideoPanel")) 
    shinyjs::toggle(id='TreeStatus', condition=input$MapGroup=='trees')
    ### Graphs
    shinyjs::onclick(id="densGraphButton", expr=shinyjs::toggle(id="GraphOptionsPanel"))
    shinyjs::onclick(id="CloseDisplayOptions", expr= shinyjs::toggle(id="GraphOptionsPanel"))
    shinyjs::onclick(id="IVGraphButton", expr=shinyjs::toggle(id="IVOptionsPanel"))
    shinyjs::onclick(id="CloseIVDisplayOptions", expr= shinyjs::toggle(id="IVOptionsPanel"))
  })
  
  
  ####  Map Panel  ####
  
  
  #### UI Controls ####
  
  # Zoom control and zoom for map 
  
  output$ParkZoomControl<-shiny::renderUI({
    shiny::selectInput(inputId="ParkZoom",label=NULL,selectize=FALSE,
                       choices=base::c("All Parks"=NETWORK,PARKLIST) )
  })
  
  
  #  Park Filter for species list control for map 
  
  output$MapParkControl<-shiny::renderUI({
    shiny::selectInput(inputId="MapPark", label="Filter species list by park",
                       choices=base::c("All Parks"="All",PARKLIST)
                       # , selected = "ANTI"
    )
  })
  
  # Data to display control for Map 
  ValuesUse<-shiny::reactive({
    base::switch(input$MapGroup,
                 trees=,saplings=base::c(Abundance="count", "Basal Area"="size"),
                 seedlings=,shseedlings=,shrubs=,vines=base::c(Abundance="count"),
                 cwd=base::c("Volume"="size"),
                 herbs=base::c("Percent Cover"="size")
                 
    )
  })
  
  output$PlantValueControl<-shiny::renderUI({
    shiny::req(ValuesUse())
    shiny::selectInput(inputId="MapValues", label="Data to Map:", choices=ValuesUse())
    
  })
  
  
  #### Calculations ####
  # Load Layers
  # withProgress(message="Loading...Please Wait", value=1,{
  #   Ecoregion<-rgdal::readOGR(dsn="./Maps/Ecoregion.geojson")#,"OGRGeoJSON")
  #   Forested<-rgdal::readOGR(dsn="./Maps/Forests.geojson")#,"OGRGeoJSON")
  #   Soil<-rgdal::readOGR(dsn="./Maps/Soils.geojson")#,"OGRGeoJSON")
  # })
  shiny::withProgress(message="Loading...Please Wait", value=1,{
    Ecoregion<-sf::st_read(dsn="./Maps/Ecoregion.geojson", quiet= TRUE)
    Forested<-sf::st_read(dsn="./Maps/Forests.geojson", quiet = TRUE)
    Soil<-sf::st_read(dsn="./Maps/Soils.geojson", quiet = TRUE)
  })
  
  #Map Cycles
  
  output$MapCycleControl<-shiny::renderUI({
    shiny::req(DATACYCLES)
    shiny::selectInput(inputId="MapCycles", label="Display data from years:", 
                       choices=base::rev(stats::setNames(base::as.character(DATACYCLES$Cycle), base::paste0(DATACYCLES$Name,":",
                                                                                                            DATACYCLES$YearStart,"-",DATACYCLES$YearEnd)))
    )
  })
  
  MapYears <- shiny::reactive({
    shiny::req(input$MapCycles)
    year_start <- DATACYCLES %>% dplyr::filter(Cycle == input$MapCycles) %>% dplyr::pull(YearStart)
    year_end <- DATACYCLES %>% dplyr::filter(Cycle == input$MapCycles) %>% dplyr::pull(YearEnd)
    
    available_years <- base::sort(base::unique(
      NPSForVeg::getEvents(object = VEGDATA, plot.type = "all")$Event_Year
    ))
    
    all_years <- year_start:year_end
    all_years[all_years %in% available_years]
  })
  
  #Build warning label for plots removed by filter selecitons
  AllPlotsCount <- shiny::reactive({
    shiny::req(MapYears())
    
    P <- NPSForVeg::getPlots(
      VEGDATA,
      years = MapYears(),
      output = "dataframe",
      type = "all")
    
    base::nrow(P)
  })
  
  # Map MetaData
  MapMetaData<-shiny::reactive({
    shiny::req(input$MapValues, input$MapGroup)
    MAPLEGEND[[input$MapValues]][[input$MapGroup]] 
  })
  
  
  
  
  
  
  
  
  ### all plots setting
  AllPlotLocations <- shiny::reactive({
    all_plots <- base::lapply(base::names(VEGDATA), function(park) {
      base::tryCatch(
        NPSForVeg::getPlots(VEGDATA[[park]], output = "dataframe", type = "all") %>%
          dplyr::select(Plot_Name, Unit_Code, Latitude, Longitude),
        error = function(e) NULL
      )
    })
    dplyr::bind_rows(all_plots[!base::sapply(all_plots, base::is.null)]) %>%
      dplyr::distinct(Plot_Name, .keep_all = TRUE)
  })
  
  showAllPlots <- shiny::reactiveVal(TRUE)
  
  # Track whether a reset is in progress
  resetting <- shiny::reactiveVal(FALSE)
  
  shiny::observeEvent(
    base::list(
      input$MapGroup,
      input$TreeStatus,
      input$MapValues,
      input$MapCycles,
      input$MapSpecies,
      input$MapPark
    ),
    {
      # Do nothing if a reset is in progress
      if (resetting()) base::return()
      
      # Do nothing if all inputs still match defaults
      default_cycle <- base::as.character(DATACYCLES$Cycle[base::nrow(DATACYCLES)])
      
      is_default <- 
        base::identical(input$MapGroup, "trees") &&
        base::identical(input$TreeStatus, "alive") &&
        base::identical(input$MapValues, "count") &&
        base::identical(input$MapCycles, default_cycle) &&
        base::identical(input$MapSpecies, "All") &&
        base::identical(input$MapPark, "All")
      
      if (!is_default) {
        showAllPlots(FALSE)
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )
  
  output$mapModeIndicator <- shiny::renderUI({
    if (showAllPlots()) {
      htmltools::tags$div(
        style = "padding: 6px 12px; margin-bottom: 8px;
               background-color: #e8f5e9; color: #2e7d32;
               border: 1.5px solid #a5d6a7; border-radius: 6px;
               font-size: 13px; font-weight: bold;",
        "\u25cf  Showing every NCRN plot ever sampled \u2014 no filters below are applied"
      )
    } else {
      shiny::actionButton(
        inputId = "resetMapFilters",
        label = "\u25cf  Showing filtered plots \u2014 Click here to show every NCRN plot ever sampled",
        style = "width: 100%; text-align: left;
                 padding: 6px 12px; margin-bottom: 8px;
                 background-color: #e3f2fd; color: #1565c0;
                 border: 1.5px solid #90caf9; border-radius: 6px;
                 font-size: 13px; font-weight: bold;
                 cursor: pointer;"
      )
    }
  })
  
  shiny::observeEvent(input$resetMapFilters, {
    resetting(TRUE)
    
    shiny::updateSelectInput(session, "MapGroup", selected = "trees")
    shiny::updateSelectInput(session, "MapValues", selected = "count")
    shiny::updateSelectInput(session, "MapPark", selected = "All")
    shiny::updateSelectInput(session, "MapSpecies", selected = "All")
    shiny::updateRadioButtons(session,"TreeStatus", selected = "alive")
    shiny::updateSelectInput(session, "MapCycles",
                             selected = base::as.character(DATACYCLES$Cycle[base::nrow(DATACYCLES)]))
    
    showAllPlots(TRUE)
    
    session$onFlushed(function() {
      resetting(FALSE)
    }, once = TRUE)
  })
  
  
  
  
  
  
  # Data to plot on map - always for all parks 
  
  ### debug ####################################################################
  
  MapData<-shiny::reactive({
    shiny::req(input$MapSpecies=="All" | input$MapSpecies %in% NPSForVeg::getPlants(object=VEGDATA, group=input$MapGroup, years=MapYears())$Latin_Name )
    shiny::req(input$MapGroup!="vines" | (input$MapGroup=="vines" & input$MapValues=="count"))
    
    P <- dplyr::left_join(
      NPSForVeg::getPlots(VEGDATA, years=MapYears(), output="dataframe", type="all") %>%
        dplyr::select(Plot_Name, Unit_Code, Latitude, Longitude),
      NPSForVeg::getEvents(object=VEGDATA, years=MapYears(), plot.type="all") %>%
        dplyr::select(Plot_Name, Year=Event_Year),
      by="Plot_Name"
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Size = if (Unit_Code %in% base::names(VEGDATA)) {
        NPSForVeg::getArea(VEGDATA[[Unit_Code]], group=input$MapGroup)
      } else {
        NA_real_
      }) %>%
      dplyr::ungroup()
    
    # if(input$MapGroup != "herbs"){
    #   base::return(P %>% 
    #    dplyr::left_join(NPSForVeg::SiteXSpec(object=VEGDATA, group=input$MapGroup, years=MapYears(), 
    #      status=if(input$MapGroup=='trees') {
    #      shiny::req(input$TreeStatus)
    #      input$TreeStatus
    #     } else {'alive'},
    #    species= if(input$MapSpecies=="All") NA else input$MapSpecies, values=input$MapValues, area="ha") %>% 
    #     dplyr::select(Plot_Name,Values=Total), by="Plot_Name")
    #   )
    # }
    
    if (input$MapGroup != "herbs") {
      
      status_val <- if (input$MapGroup == "trees") {
        shiny::req(input$TreeStatus)
        input$TreeStatus
      } else {
        "alive"
      }
      
      species_val <- if (input$MapSpecies == "All") NA else input$MapSpecies
      
      if (input$MapPark == "All") {
        results <- base::lapply(base::names(VEGDATA), function(park) {
          base::tryCatch(
            NPSForVeg::SiteXSpec(
              object = VEGDATA[[park]],
              group = input$MapGroup,
              years = MapYears(),
              status = status_val,
              species = species_val,
              values = input$MapValues,
              area = "ha"
            ),
            error = function(e) {
              base::message("SiteXSpec failed for park: ", park, " - ", e$message)
              NULL
            }
          )
        })
        spec_data <- dplyr::bind_rows(results[!base::sapply(results, base::is.null)])
        } else {

        spec_data <- base::tryCatch(
          NPSForVeg::SiteXSpec(
            object = VEGDATA[[input$MapPark]],
            group = input$MapGroup,
            years = MapYears(),
            status = status_val,
            species = species_val,
            values = input$MapValues,
            area = "ha"
          ),
            error = function(e) {
              base::message("SiteXSpec failed for park: ", input$MapPark, " - ", e$message)
              NULL
            }
          )
        shiny::validate(shiny::need(
          !base::is.null(spec_data),
          base::paste("No data found for this species/group/year combination in",
                      NPSForVeg::getNames(VEGDATA[[input$MapPark]], "long"), ".")
        ))
      }
      
      base::return(P %>% dplyr::left_join(spec_data %>% dplyr::select(Plot_Name, Values = Total), by = "Plot_Name") %>%
                     dplyr::filter(!base::is.na(Values) & Values > 0))
    }
    
    if (input$MapGroup == "herbs") {
      
      if (input$MapPark == "All") {
        results <- base::lapply(base::names(VEGDATA), function(park) {
          base::tryCatch(
            NPSForVeg::SiteXSpec(
              object = VEGDATA[[park]],
              group = input$MapGroup,
              years = MapYears(),
              species = if (input$MapSpecies == "All") NA else input$MapSpecies,
              values = input$MapValues
            ),
            error = function(e) {
              base::message("SiteXSpec failed for park: ", park, " - ", e$message)
              NULL
            }
          )
        })
        spec_data <- dplyr::bind_rows(results[!base::sapply(results, base::is.null)])
        } else {
         spec_data <- base::tryCatch(
           NPSForVeg::SiteXSpec(
            object = VEGDATA[[input$MapPark]],
            group = input$MapGroup,
             years = MapYears(),
            species = if (input$MapSpecies == "All") NA else input$MapSpecies,
            values = input$MapValues
          ),
          error = function(e) {
             base::message("SiteXSpec failed for park: ", input$MapPark, " - ", e$message)
             NULL
            }
          )
          
        shiny::validate(shiny::need(
          !base::is.null(spec_data),
           base::paste("No data found for this species/group/year combination in",
                      NPSForVeg::getNames(VEGDATA[[input$MapPark]], "long"), ".")
         ))
      }
      
      base::return(P %>% dplyr::left_join(spec_data %>% dplyr::select(Plot_Name, Values = Total), by = "Plot_Name") %>%
                     dplyr::filter(!base::is.na(Values) & Values > 0))
    }
  }) ###########################################################################
  
  # Map Colors
  CircleColors<-shiny::reactive({
    shiny::req(MapMetaData()$Cuts)
    shiny::req(!base::is.null(MapData()) && base::nrow(MapData()) > 0)
    leaflet::colorBin(palette=base::c("cyan","magenta4","orangered3"),domain=MapData()$Values, bins=base::c(MapMetaData()$Cuts+.001)) # colors for circles
  })  
  
  POLYCOLORS<-grDevices::colorRamp(base::c("aquamarine4","green","yellow","goldenrod4")) #colors for polygons
  
  
  
  #### Render Map  ####
  
  output$VegMap <- leaflet::renderLeaflet({
    bounds <- PARKBOUNDS[PARKBOUNDS$ParkCode == NETWORK, ]
    
    leaflet::leaflet(options = leafletOptions(
      scrollWheelZoom = TRUE,  # keep OFF to avoid wheel capturing page scroll
      touchZoom       = FALSE,  # keep OFF to avoid pinch-zoom trapping on mobile
      dragging        = TRUE,   # allow panning
      keyboard        = FALSE,  # optional: prevents keyboard focus hijacking
      tap             = FALSE   # optional: avoids odd tap delays on mobile
    )) %>%
      leaflet::fitBounds(
        lng1 = bounds$LongW, lat1 = bounds$LatS, lng2 = bounds$LongE, lat2 = bounds$LatN
      ) %>%
      leaflet::setView(
        lng = mean(c(bounds$LongW, bounds$LongE)),
        lat = mean(c(bounds$LatS, bounds$LatN)),
        zoom = 9
      ) %>%
      leaflet::setMaxBounds(
        lng1 = bounds$LongW, lng2 = bounds$LongE, lat1 = bounds$LatS, lat2 = bounds$LatN)})
  #       %>%
  # Optional: enable wheel zoom only while hovering (desktop), auto-disable on leave
  #      onRender("
  #      function(el, x) {
  #        var map = this;
  
  #        // Defensive: keep zoom disabled by default
  #        map.scrollWheelZoom && map.scrollWheelZoom.disable();
  #        map.touchZoom && map.touchZoom.disable();
  
  #        // Desktop: allow wheel zoom only while hovering the map
  #        el.addEventListener('mouseenter', function(){
  #          map.scrollWheelZoom && map.scrollWheelZoom.enable();
  #        }, {passive:true});
  
  #        el.addEventListener('mouseleave', function(){
  #          map.scrollWheelZoom && map.scrollWheelZoom.disable();
  #        }, {passive:true});
  
  #        // Mobile: ensure zoom capture is off when touch leaves / cancels
  #        ['touchend','pointerleave','pointercancel','blur'].forEach(function(evt){
  #          el.addEventListener(evt, function(){
  #            map.scrollWheelZoom && map.scrollWheelZoom.disable();
  #            map.touchZoom && map.touchZoom.disable();
  #          }, {passive:true});
  #        });
  
  #        // If the page starts scrolling, kill zoom capture immediately
  #        window.addEventListener('scroll', function(){
  #          map.scrollWheelZoom && map.scrollWheelZoom.disable();
  #          map.touchZoom && map.touchZoom.disable();
  #        }, {passive:true});
  #      }
  #    ")
  #  })
  
  
  # Make Attribution
  NPSATTRIB<-htmltools::HTML("<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> | 
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles' 
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>")
  
  
  # add Monitoring plot data as circles
  shiny::observe({
    if (showAllPlots()) {
      all_df <- AllPlotLocations()
      shiny::req(!base::is.null(all_df) && base::nrow(all_df) > 0)
      
      leaflet::leafletProxy("VegMap") %>%
        leaflet::clearGroup("Circles") %>%
        leaflet::addCircles(
          data = all_df,
          radius = 15 * base::as.numeric(input$PlotSize),
          group = "Circles",
          lng = all_df$Longitude,
          lat = all_df$Latitude,
          layerId = all_df$Plot_Name,
          fillColor = "green",
          color = "green",
          fillOpacity = 0.7
        )
      
    } else {
    shiny::req(!base::is.null(MapData()) && base::nrow(MapData()) > 0)
    input$MapLayer #make sure Circles are always on top
    
    leaflet::leafletProxy("VegMap") %>%
      leaflet::clearGroup("Circles") %>%
      leaflet::addCircles(data=MapData(), radius=15*base::as.numeric(input$PlotSize), group="Circles",
                          lng=MapData()$Longitude, lat=MapData()$Latitude,
                          layerId=MapData()$Plot_Name,  #This is the ID of the circle to match to other data
                          fillColor=CircleColors()(MapData()$Values),
                          color=CircleColors()(MapData()$Values),
                          fillOpacity=1
      )
    }
  })
  
  
  # #Add a tile layer
  shiny::observe({
    leaflet::leafletProxy("VegMap") %>%
      leaflet::clearTiles() %>%
      
      leaflet::addTiles(group="Map", urlTemplate=NPSBASIC, attribution=NPSATTRIB, options=leaflet::tileOptions(minZoom=1))%>%
      leaflet::addTiles(group="Imagery", urlTemplate=NPSIMAGERY,attribution=NPSATTRIB, options=leaflet::tileOptions(minZoom=1)) %>%
      leaflet::addTiles(group="Light", urlTemplate = NPSLIGHT, attribution=NPSATTRIB, options=leaflet::tileOptions(minZoom=1)) %>% 
      leaflet::addTiles(group="Slate", urlTemplate=NPSSLATE, attribution=NPSATTRIB, options=leaflet::tileOptions(minZoom=1)) %>%
      leaflet::addLayersControl(map=., baseGroups=base::c("Map","Imagery","Light","Slate"), options=leaflet::layersControlOptions(collapsed=T))
  }) #urlTemplate comes from secrets.R file
  
  # Add Circle legends 
  shiny::observe({
    if (showAllPlots()) {
      leaflet::leafletProxy("VegMap") %>%
        leaflet::removeControl(layerId = "CircleLegend")
    } else {
      shiny::req(CircleColors())
      leaflet::leafletProxy("VegMap") %>%
        leaflet::removeControl(layerId = "CircleLegend") %>%
        leaflet::addLegend(
          title   = MapMetaData()$Title,
          colors  = CircleColors()(MapMetaData()$Cuts[-1] - .001),
          labels  = MapMetaData()$Labels,
          layerId = "CircleLegend",
          opacity = 1
        )
    }
  })
  
  
  # Species list control for map 
  
  ### function to get common names ###
  
  # getCNdf <- function(tsn_list) {
  #   # Defines a function to search a list of TSNs in ITIS, define one common name per TSN, and organize the output into a
  #   # data.frame of TSNs and common names.
  #   # 
  #   # Args:
  #   #   tsn_list, list, required. A list of TSNs corresponding to the selected park(s)
  #   #
  #   # Returns:
  #   #   cn_df, df. A data.frame containing:
  #   #     TSN, chr. A column of the TSNs contained in tsn_list.
  #   #     Common, chr. A column of corresponding common names dplyr::filtered from ITIS output data.
  #   # 
  #   # Example:
  #   #   returnCNdf <- shiny::reactive({
  #   #     tsn_list <- getTSNlist()
  #   #     cn_df <- getCNdf(tsn_list)
  #   #     base::return(cn_df)
  #   #   })
  #   
  #   cn_list <- base::list()
  #   for (i in seq_along(tsn_list)) {
  #     tsn <- tsn_list[i]
  #     cn <- base::tryCatch(
  #       ritis::common_names(tsn),
  #       error = function(e) NULL)
  #     cn <- cn %>%
  #       dplyr::filter(language %in% base::c("English", "unspecified")) %>%
  #       dplyr::dplyr::mutate(name_word_count = base::lengths(base::strsplit(commonName, "\\s+"))) %>%
  #       dplyr::filter(!(name_word_count == 1 & base::any(name_word_count >= 2))) %>%
  #       dplyr::slice(1)
  #     
  #     cn_list[[i]] <- cn
  #   }
  #   
  #   cn_df <- base::do.call(base::rbind, base::lapply(cn_list, base::as.data.frame))
  #   
  #   if (base::nrow(cn_df) == 0) base::return(NULL)
  #   
  #   cn_df <- cn_df %>% 
  #     dplyr::select(-name_word_count, -language) %>%
  #     dplyr::rename(TSN = tsn) %>%
  #     dplyr::rename(Common = commonName)
  #   
  #   base::return(cn_df)
  # }
  # ### extract MapGroup input (Trees) from VEGDATA for all Parks ###
  # 
  selected_object <- shiny::reactive({
    # Defines a reactive function based on user inputs to MapGroup and MapPark that binds together dfs from various slots of
    # VEGDATA to create a data object containing relevant TSNs and Latin Names. For example, if 'Trees' and 'All parks' are
    # selected, the trees data from all 11 slots in VEGDATA will be bound as a single data.frame.
    #
    # Args:
    #   VEGDATA, NPSForVeg S4 object, required.
    #
    # Returns:
    #   vegdata_df, data.frame.
    #
    # Example:
    #   vegdata_df <- selected_object()
    
    shiny::req(input$MapGroup)
    shiny::req(input$MapPark)
    
    actual_slot_name <- PLANTSLOTLOOKUP[[input$MapGroup]]
    shiny::validate(
      shiny::need(!base::is.null(actual_slot_name), "Selected plant group is not available in this network")
    )
    
    if (input$MapPark == "All") {
      selected_list <- VEGDATA
    } else {
      selected_list <- base::list(VEGDATA[[input$MapPark]])
    }
    
    vegdata_df <- selected_list %>%
      base::lapply(function(obj) methods::slot(obj, actual_slot_name)) %>%
      dplyr::bind_rows() %>%
      base::as.data.frame() %>%
      dplyr::filter(Latin_Name != "Unknown")
    
    base::return(vegdata_df)
  })
  # 
  # getTSNlist <- shiny::reactive({
  #   # Defines a reactive function which creates a list of unique and valid TSNs from a previously created data object.
  #   # 
  #   # Args:
  #   #   selected_object(), function, required. Returns vegdata_df data.frame.
  #   # 
  #   # Returns:
  #   #   tsn_list, list. Contains valid TSNs from selected data as characters.
  #   # 
  #   # Example:
  #   #   tsn_list <- getTSNlist()
  #   
  #   vegdata_df <- selected_object()
  #   
  #   tsn_list <- base::as.character(base::unique(vegdata_df$TSN))
  #   bad_tsns <- base::c("19243", "-400000")   # may have to update with non-Tree bad TSNs
  #   tsn_list <- tsn_list[!tsn_list %in% bad_tsns]
  #   
  #   base::return(tsn_list)
  # })
  #   
  # returnCNdf <- shiny::reactive({
  #   # Applies the getCNdf() reactive function to the tsn_list output of getTSNlist() to create cn_df data.frame.
  #   # 
  #   # Args:
  #   #   getTSNlist(), function, required. Returns tsn_list.
  #   # 
  #   # Returns:
  #   #   cn_df, data.frame. Containing columns of TSNs from tsn_list and corresponding common names from ITIS.
  #   # 
  #   # Example:
  #   #   cn_df <- returnCNdf()
  #   
  #   tsn_list <- getTSNlist()
  #   cn_df <- getCNdf(tsn_list)
  #   
  #   base::return(cn_df)
  # })  
  # 
  # ### generate lookup table for getPlantNames ###
  # 
  # plants_lookup <- shiny::reactive({
  #   # Defines a reactive function to merge cn_df into vegdata_df by TSN columns, adding a column of common names to the selected data
  #   # object. Then dplyr::filters the merged df into a plants look-up df by keeping only rows with unique combinations of Latin and common
  #   # names while removing other columns of data.
  #   # 
  #   # Args:
  #   #   selected_object(), function, required. Returns vegdata_df data.frame.
  #   #   returnCNdf(), function, required. Returns cn_df data.frame.
  #   # 
  #   # Returns:
  #   #   plants_lookup, data.frame. Containing corresponding Common and Latin_Name columns.
  #   # 
  #   # Example:
  #   #   plants_lookup <- plants_lookup()
  #   
  #   shiny::req(input$MapGroup)
  #   shiny::req(input$MapPark)
  #   
  #   actual_slot_name <- PLANTSLOTLOOKUP[[input$MapGroup]]
  #   shiny::validate(
  #     shiny::need(!base::is.null(actual_slot_name), "Selected plant group is not available in this network")
  #   )
  #   
  #   vegdata_df <- selected_object()
  #   vegdata_df$TSN <- base::as.character(vegdata_df$TSN)
  #   
  #   cn_df <- returnCNdf()
  #   cn_df$TSN <- base::as.character(cn_df$TSN)
  #   
  #   plants_merged <- dplyr::dplyr::left_join(vegdata_df, cn_df, by = "TSN")
  #   plants_lookup <- plants_merged %>%
  #     dplyr::distinct(Latin_Name, Common, .keep_all = FALSE) %>%
  #     dplyr::dplyr::mutate(Common = base::ifelse(Latin_Name == "Carya ovata", "shagbark hickory", Common)) %>%
  #     dplyr::dplyr::mutate(Common = base::ifelse(Latin_Name == "Pyrus betulifolia","birchleaf pear", Common))
  #   
  #   base::return(plants_lookup)
  # })  
  
  
  
  ### Now that I realize CommonNames.csv exists... ###
  getCNdf <- function(tsn_list) {
    # Defines a function to search a list of TSNs in ITIS, define one common name per TSN, and organize the output into a
    # data.frame of TSNs and common names.
    # 
    # Args:
    #   tsn_list, list, required. A list of TSNs corresponding to the selected park(s)
    #
    # Returns:
    #   cn_df, df. A data.frame containing:
    #     TSN, chr. A column of the TSNs contained in tsn_list.
    #     Common, chr. A column of corresponding common names dplyr::filtered from ITIS output data.
    # 
    # Example:
    #   returnCNdf <- shiny::reactive({
    #     tsn_list <- getTSNlist()
    #     cn_df <- getCNdf(tsn_list)
    #     base::return(cn_df)
    #   })
    
    cn_list <- base::list()
    for (i in seq_along(tsn_list)) {
      tsn <- tsn_list[i]
      cn <- base::tryCatch(
        ritis::common_names(tsn),
        error = function(e) NULL)
      if (base::is.null(cn) || base::nrow(cn) == 0) {
        cn_list[[i]] <- base::data.frame(
          tsn = tsn,
          commonName = NA_character_,
          language = NA_character_,
          stringsAsFactors = FALSE
        )
        next
      }
      
      cn <- cn %>%
        dplyr::filter(language %in% base::c("English", "unspecified")) %>%
        dplyr::mutate(name_word_count = base::lengths(base::strsplit(commonName, "\\s+")))
      
      # Guard: check again after language filter
      if (base::nrow(cn) == 0) {
        cn_list[[i]] <- base::data.frame(
          tsn = tsn,
          commonName = NA_character_,
          language = NA_character_,
          stringsAsFactors = FALSE
        )
        next
      }
      
      cn <- cn %>%
        dplyr::filter(!(name_word_count == 1 & base::any(name_word_count >= 2))) %>%
        dplyr::slice(1)
      
      cn$commonName <- fmt_common(cn$commonName)
      
      cn_list[[i]] <- cn
    }
    
    cn_df <- base::do.call(base::rbind, base::lapply(cn_list, base::as.data.frame))
    
    if (base::is.null(cn_df) || base::nrow(cn_df) == 0) base::return(NULL)
    
    cn_df <- cn_df %>% 
      dplyr::select(-name_word_count, -language) %>%
      dplyr::rename(TSN = tsn) %>%
      dplyr::rename(Common = commonName)
    
    base::return(cn_df)
  }
  
  selected_commons <- shiny::reactive({
    
    shiny::req(input$MapGroup)
    shiny::req(input$MapPark)
    
    # actual_slot_name <- PLANTSLOTLOOKUP[[input$MapGroup]]
    # shiny::validate(
    #   shiny::need(!base::is.null(actual_slot_name), "Selected plant group is not available in this network")
    # )
    
    # output does not change based on input to MapPark because CommonNames.csv is identical for each park
    # if (input$MapPark == "All") {
    #   selected_list <- VEGDATA
    # } else {
    #   selected_list <- base::list(VEGDATA[[input$MapPark]])
    # }
    
    selected_list <- VEGDATA
    vegdata_df <- selected_list[[1]]@Commons
    
    base::return(vegdata_df)
  })
  
  get_TSNs <- shiny::reactive({
    
    vegdata_df <- selected_commons()
    tsn_df <- vegdata_df %>%
      dplyr::distinct(TSN, Common, .keep_all = TRUE) %>%
      dplyr::filter(Common == "")
    tsn_list <- tsn_df$TSN
    tsn_list <- tsn_list[tsn_list != "25328"]
    
    base::return(tsn_list)
  })
  
  return_cndf <- shiny::reactive ({
    
    tsn_list <- get_TSNs()
    cn_df <- getCNdf(tsn_list)
    
    base::return(cn_df)
  })
  
  get_vd_filled <- shiny::reactive({
    
    vegdata_df <- selected_commons()
    cn_df <- return_cndf()
    if (base::is.null(cn_df)) {
      vd_filled <- vegdata_df %>%
        dplyr::distinct(Latin_Name, Common, .keep_all = FALSE)
    } else {
      vd_filled <- vegdata_df %>%
        dplyr::left_join(cn_df, by = "TSN", suffix = base::c("", "_new")) %>%
        dplyr::mutate(
          Common = base::ifelse(Common == "" | base::is.na(Common), fmt_common(Common_new), fmt_common(Common))
        ) %>%
        dplyr::select(-Common_new) %>%
        dplyr::distinct(Latin_Name, Common, .keep_all = FALSE)
    }
    
    #vd_filled <- vd_filled %>%
    #  dplyr::mutate(Common = base::ifelse(TSN == "25328", "spirea", Common))
    
    new_vd_rows <- base::data.frame(
      Latin_Name = base::c("Acer spp.", "Quercus acutissima", "Oplismenus undulatifolius", "Robinia viscosa", "Viburnum lantana", "Rosaceae Family", "Lygodium palmatum"),
      Common = base::c("Maples", "Sawtooth oak", "Wavyleaf basketgrass", "Clammy locust", "Wayfaring tree", "Roses", "American climbing fern")
    )
    vd_filled <- base::rbind(vd_filled, new_vd_rows)
    
    base::return(vd_filled)
  })
  
  #List of names, elements are Latin names, names of elements are Latin or common
  MapSpecList<-shiny::reactive({
    shiny::req(input$MapPark, input$MapGroup)
    SpecTemp<-base::unique(NPSForVeg::getPlants(object=if(input$MapPark=="All") {VEGDATA}  else {VEGDATA[[input$MapPark]]} , group=input$MapGroup,
                                                years=MapYears(),common=F )$Latin_Name)
    vd_filled<-get_vd_filled()
    safeGetPlantNames <- function(object, names, in.style, out.style) {
      base::tryCatch({
        NPSForVeg::getPlantNames(object = object, names = names, in.style = in.style, out.style = out.style)
      }, 
      error = function(e) {
        available_names <- names[names %in% object$Latin_Name]
        removed_names <- base::setdiff(names, available_names)
        message("The following taxa were not found and were removed: ",
                base::paste(removed_names, collapse = ", "))
        
        # Try again with only the valid names
        NPSForVeg::getPlantNames(object = object, names = available_names, in.style = in.style, out.style = out.style)
        
      })
    }
    
    SpecNames <- fmt_common(safeGetPlantNames(object = vd_filled %>%
                                     dplyr::distinct(Latin_Name, .keep_all = TRUE),
                                   names = SpecTemp,
                                   in.style = "Latin",
                                   out.style = base::ifelse(input$mapCommon, "common", "Latin")))
    
    base::names(SpecTemp)<-SpecNames
    SpecTemp<-SpecTemp[order(base::tolower(base::names(SpecTemp)))]
    SpecTemp<-base::c("All Species"="All", SpecTemp)
  })

  
  # MapSpecList<-shiny::reactive({
  #   shiny::req(input$MapPark, input$MapGroup)
  #   SpecTemp<-base::unique(NPSForVeg::getPlants(object=if(input$MapPark=="All") {VEGDATA}  else {VEGDATA[[input$MapPark]]} , group=input$MapGroup,
  #                              years=MapYears(),common=F )$Latin_Name)
  #   vd_filled <- get_vd_filled()
  #     
  #   SpecNames<-NPSForVeg::getPlantNames(object=vd_filled, names=SpecTemp, in.style="Latin",out.style=base::ifelse(input$mapCommon,"common","Latin"))
  #  base::names(SpecTemp)<-SpecNames
  #   SpecTemp<-SpecTemp[order(base::tolower(base::names(SpecTemp)))]
  #   SpecTemp<-base::c("All Species"="All", SpecTemp)
  # })
  
  output$MapSpeciesControl<-shiny::renderUI({
    shiny::req(input$MapPark, input$MapGroup)
    shiny::selectInput(inputId="MapSpecies", label="Select a species", choices=base::c(MapSpecList() ))
    
  })
  
  
  # Add GeoJSON polygon layer 
  
  shiny::observe({
    leaflet::leafletProxy("VegMap") %>% {
      base::switch(input$MapLayer,
                   None=leaflet::clearGroup(.,group=base::c("Ecoregion","Forested","Soil")) %>% leaflet::removeControl(.,"LayerLegend"),
                   
                   EcoReg=leaflet::clearGroup(.,group=base::c("Forested","Soil") )%>% 
                     leaflet::addPolygons(., data=Ecoregion, group="Ecoregion", layerId=Ecoregion$MapClass, 
                                          stroke=FALSE, 
                                          fillOpacity=.65, color=leaflet::colorFactor(palette=POLYCOLORS, levels=Ecoregion$MapClass)(Ecoregion$MapClass)),
                   
                   ForArea=leaflet::clearGroup(.,group=base::c("Ecoregion","Soil")) %>% 
                     leaflet::addPolygons(.,data=Forested, group="Forested", layerId=Forested$MapClass, stroke=FALSE, 
                                          fillOpacity=.65, color=leaflet::colorFactor("Greens",levels=Forested$MapClass)(Forested$MapClass)),
                   
                   Soil=leaflet::clearGroup(.,group=base::c("Ecoregion","Forested")) %>% 
                     leaflet::addPolygons(.,data=Soil, group="Soil", layerId=Soil$MapClass, stroke=FALSE, 
                                          fillOpacity=.65, color=leaflet::colorFactor(POLYCOLORS,levels=Soil$MapClass)(Soil$MapClass)) 
      )}
  })
  
  # Zoom the map
  
  shiny::observeEvent(input$MapZoom, {
    BoundsUse<-shiny::reactive({ base::as.numeric(PARKBOUNDS[PARKBOUNDS$ParkCode==input$ParkZoom,2:5]) })
    leaflet::leafletProxy("VegMap") %>% leaflet::fitBounds(lat1=BoundsUse()[1], lng1=BoundsUse()[2], lat2=BoundsUse()[3], lng2=BoundsUse()[4])
  })
  
  
  # Add layer legends 
  
  shiny::observe({
    leaflet::leafletProxy("VegMap") %>%  leaflet::removeControl(layerId="LayerLegend") %>%
      { base::switch(input$MapLayer,
                     None=NA,
                     EcoReg= leaflet::addLegend(.,title="Layer Legend",pal=leaflet::colorFactor(POLYCOLORS, levels=Ecoregion$MapClass),
                                                values=Ecoregion$MapClass, layerId="LayerLegend"),
                     
                     ForArea= leaflet::addLegend(.,title="Layer Legend",pal=leaflet::colorFactor("Greens",levels=Forested$MapClass),
                                                 values=Forested$MapClass,layerId="LayerLegend"),
                     Soil= leaflet::addLegend(.,title="Layer Legend",pal=leaflet::colorFactor(POLYCOLORS, levels=Soil$MapClass),
                                              values=Soil$MapClass, layerId="LayerLegend")
      )}
  })
  
  # Mouse Hover 
    shiny::observeEvent(input$VegMap_shape_mouseover, {
    ShapeOver <- input$VegMap_shape_mouseover
    
    if (showAllPlots()) {
      all_df <- AllPlotLocations()
      selectedPlot <- all_df[all_df$Plot_Name == ShapeOver$id, ]
      if (base::nrow(selectedPlot) == 0) base::return()
      leaflet::leafletProxy("VegMap") %>%
        leaflet::clearPopups() %>%
        leaflet::addPopups(
          map     = .,
          lat     = ShapeOver$lat + .001,
          lng     = ShapeOver$lng,
          layerId = "MouseOverPopup",
          popup   = base::paste0(
            shiny::h5(NPSForVeg::getNames(VEGDATA[[selectedPlot$Unit_Code]], "long")),
            shiny::h6("Monitoring Plot: ", selectedPlot$Plot_Name),
            htmltools::tags$h6("Use filters to see data for this plot")
          )
        )
      base::return()
    }
    
    selectedPlot <- MapData()[MapData()$Plot_Name == ShapeOver$id, ]
    if (base::nrow(selectedPlot) == 0) base::return()
    leaflet::leafletProxy("VegMap") %>%
      leaflet::clearPopups() %>% {
        base::switch(ShapeOver$group,
                     Circles = leaflet::addPopups(
                       map     = .,
                       lat     = ShapeOver$lat + .001,
                       lng     = ShapeOver$lng,
                       layerId = "MouseOverPopup",
                       popup   = base::paste0(
                         shiny::h5(NPSForVeg::getNames(VEGDATA[[selectedPlot$Unit_Code]], "long")),
                         shiny::h6("Monitoring Plot:", selectedPlot$Plot_Name),
                         shiny::h6("Year Monitored:", selectedPlot$Year),
                         shiny::h6(
                           base::names(MapSpecList()[MapSpecList() == input$MapSpecies]), ":",
                           base::format(base::signif(selectedPlot$Values, 2), big.mark = ","),
                           " ", MapMetaData()$Title),
                         htmltools::tags$h6("Click on plot to see full list")
                       )
                     )
        )
      }
  })
  
  shiny::observeEvent(input$VegMap_shape_mouseout,{    #clear popup when mouse leaves circle
    leaflet::leafletProxy("VegMap") %>%
      leaflet::clearPopups()
  })
  
  # Mouse Click 
  
  # REPLACE the entire observeEvent(input$VegMap_shape_click block WITH:
  shiny::observeEvent(input$VegMap_shape_click, {
    ShapeClick <- input$VegMap_shape_click
    
    if (showAllPlots()) {
      all_df <- AllPlotLocations()
      selectedPlot <- all_df[all_df$Plot_Name == ShapeClick$id, ]
      if (base::nrow(selectedPlot) == 0) base::return()
      leaflet::leafletProxy("VegMap") %>%
        leaflet::clearPopups() %>%
        leaflet::addPopups(
          map     = .,
          lat     = ShapeClick$lat + .001,
          lng     = ShapeClick$lng,
          layerId = "CircleClickPopup",
          popup   = base::paste0(
            shiny::h5(NPSForVeg::getNames(VEGDATA[[selectedPlot$Unit_Code]], "long")),
            shiny::h6("Monitoring Plot: ", selectedPlot$Plot_Name),
            htmltools::tags$h6("Use the filters to see species data for this plot")
          )
        )
      base::return()
    }
    
    selectedPlot <- MapData()[MapData()$Plot_Name == ShapeClick$id, ]
    if (base::nrow(selectedPlot) == 0) base::return()
    
    if (base::class(base::try(
      NPSForVeg::SiteXSpec(
        object  = VEGDATA[[selectedPlot$Unit_Code]],
        group   = input$MapGroup,
        years   = selectedPlot$Year,
        plots   = ShapeClick$id,
        common  = input$mapCommon,
        status  = if (input$MapGroup == "trees") input$TreeStatus else "alive"
      ), silent = TRUE)) == "try-error") {
      content <- base::as.character(htmltools::tagList(
        htmltools::tags$h6("None found on this plot")))
    } else {
      tempData <- if (input$MapGroup != "herbs") {
        NPSForVeg::SiteXSpec(
          object  = VEGDATA[[selectedPlot$Unit_Code]],
          group   = input$MapGroup,
          years   = selectedPlot$Year,
          plots   = ShapeClick$id,
          values  = input$MapValues,
          area    = "ha",
          common  = input$mapCommon,
          status  = if (input$MapGroup == "trees") input$TreeStatus else "alive"
        )[-1]
      } else {
        NPSForVeg::SiteXSpec(
          object = VEGDATA[[selectedPlot$Unit_Code]],
          group  = input$MapGroup,
          years  = selectedPlot$Year,
          plots  = ShapeClick$id,
          values = input$MapValues,
          common = input$mapCommon
        )[-1]
      }
      base::names(tempData) <- fmt_common(base::names(tempData))
      content <- base::paste0(
        shiny::h5(NPSForVeg::getNames(VEGDATA[[selectedPlot$Unit_Code]], "long")),
        shiny::h6("Monitoring Plot:", selectedPlot$Plot_Name),
        shiny::h6("Year Monitored:", selectedPlot$Year),
        shiny::h6("Species: ", MapMetaData()$Title),
        htmltools::tagList(htmltools::tags$table(
          base::mapply(
            FUN = function(Name, Value) {
              htmltools::tags$tr(
                htmltools::tags$td(base::sprintf("%s:  ", Name)),
                htmltools::tags$td(align = "right",
                                   base::sprintf("%s", base::format(base::signif(Value, 2), big.mark = ",")))
              )
            },
            Name     = base::names(tempData),
            Value    = base::unlist(tempData),
            SIMPLIFY = FALSE
          )))
      )
    }
    leaflet::leafletProxy("VegMap") %>%
      leaflet::clearPopups() %>% {
        base::switch(ShapeClick$group,
                     Circles   = leaflet::addPopups(map = ., lat = ShapeClick$lat + .001,
                                                    lng = ShapeClick$lng, layerId = "CircleClickPopup", popup = content),
                     Ecoregion =,
                     Forested  =,
                     Soil      = leaflet::addPopups(map = ., lat = ShapeClick$lat,
                                                    lng = ShapeClick$lng, popup = ShapeClick$id)
        )
      }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # #Build warning label
  # disableWarnings <- shiny::reactive({
  #   
  #   species <- input$MapSpecies
  #   park <- input$MapPark
  #   
  #   isTRUE(species == "All") && isTRUE(park == "All")
  # })
  # plotCounts <- shiny::reactive({
  #   
  #   shiny::req(MapData(), MapYears())
  #   
  #   all_plots <- NPSForVeg::getPlots(
  #     VEGDATA,
  #     years = MapYears(),
  #     output = "dataframe",
  #     type = "all"
  #   )
  #   
  #   total <- nrow(all_plots)
  #   filtered <- length(unique(MapData()$Plot_Name))
  #   
  #   list(
  #     total = total,
  #     filtered = filtered,
  #     removed = total - filtered
  #   )
  # })
  # 
  # speciesWarning <- shiny::reactive({
  #   
  #   if (isTRUE(disableWarnings())) return(NULL)
  #   shiny::req(plotCounts())
  #   
  #   pc <- plotCounts()
  #   
  #   spec_list <- MapSpecList()
  #   
  #   species_name <- if (input$MapSpecies %in% spec_list) {
  #     names(spec_list)[spec_list == input$MapSpecies]
  #   } else {
  #     input$MapSpecies
  #   }
  #   
  #   verb <- if (identical(input$MapSpecies, "All")) "have" else "has"
  #   
  #   paste0(
  #     species_name, " ", verb,
  #     " been observed by NCRN at ", pc$filtered, " plots. ",
  #     pc$removed,
  #     " points were removed from the map because NCRN has no recorded observations of ",
  #     tolower(species_name),
  #     " under the selected data filters."
  #   )
  # })
  # 
  # parkWarning <- shiny::reactive({
  #   
  #   if (isTRUE(disableWarnings())) return(NULL)
  #   shiny::req(plotCounts())
  #   
  #   pc <- plotCounts()
  #   
  #   paste0(
  #     pc$removed,
  #     " plots have been removed from the map due to current park selection (",
  #     input$MapPark,
  #     ")."
  #   )
  # })
  # 
  # last_species_msg <- shiny::reactiveVal(NULL)
  # last_park_msg <- shiny::reactiveVal(NULL)
  # 
  # last_park <- shiny::reactiveVal(NULL)
  # 
  # observeEvent(input$MapPark, {
  #   
  #   shiny::req(MapData(), MapYears())
  #   
  #   if (isTRUE(disableWarnings())) return()
  #   
  #   pc <- plotCounts()
  #   
  #   msg <- paste0(
  #     pc$removed,
  #     " plots have been removed from the map due to current park selection (",
  #     input$MapPark,
  #     ")."
  #   )
  #   
  #   if (!identical(last_park(), msg)) {
  #     last_park(msg)
  #     
  #     showNotification(
  #       msg,
  #       id = "park_warning",
  #       type = "error",
  #       duration = NULL
  #     )
  #   }
  #   
  # }, ignoreInit = TRUE)
  # 
  # last_species <- shiny::reactiveVal(NULL)
  # 
  # observeEvent(input$MapSpecies, {
  #   
  #   if (identical(input$MapSpecies, "All")) return()
  #   if (isTRUE(disableWarnings())) return()
  #   
  #   shiny::req(MapData(), MapYears())
  #   
  #   pc <- plotCounts()
  #   
  #   spec_list <- MapSpecList()
  #   
  #   species_name <- if (input$MapSpecies %in% spec_list) {
  #     names(spec_list)[spec_list == input$MapSpecies]
  #   } else {
  #     input$MapSpecies
  #   }
  #   
  #   verb <- "has"
  #   
  #   msg <- paste0(
  #     species_name, " ", verb,
  #     " been observed by NCRN at ", pc$filtered, " plots. ",
  #     pc$removed,
  #     " points were removed from the map because NCRN has no recorded observations of ",
  #     tolower(species_name),
  #     " under the selected data filters."
  #   )
  #   
  #   if (!identical(last_species(), msg)) {
  #     last_species(msg)
  #     
  #     showNotification(
  #       msg,
  #       id = "species_warning",
  #       type = "error",
  #       duration = NULL
  #     )
  #   }
  #   
  # }, ignoreInit = TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #### Plots Tab ####
  
  #### Park Control for Density plot  ####
  output$densParkControl<-shiny::renderUI({
    shiny::selectizeInput(inputId="densPark",choices=PARKLIST, label="Park:",
                          options = base::list(placeholder='Select a park',
                                               onInitialize = base::I('function() { this.setValue(""); }') )) 
  })
  
  
  #### Dens Cycles ####
  output$densCycleControl<-shiny::renderUI({
    shiny::req(DATACYCLES)
    shiny::selectInput(inputId="densCycles", label="Display data from years:", 
                       choices=base::rev(stats::setNames(base::as.character(DATACYCLES$Cycle), base::paste0(DATACYCLES$Name,":",
                                                                                                            DATACYCLES$YearStart,"-",DATACYCLES$YearEnd)))
    )
  })
  
  #### Dens Years ####
  densYears<-shiny::reactive({
    shiny::req(input$densCycles)
    (DATACYCLES %>% dplyr::filter(Cycle==input$densCycles) %>% dplyr::pull(YearStart)) : 
      (DATACYCLES %>% dplyr::filter(Cycle==input$densCycles) %>% dplyr::pull(YearEnd))
  })
  
  
  ##### Data to display control for density plot ####
  DensValuesUse<-shiny::reactive({
    base::switch(input$densGroup,
                 trees=,saplings=base::c(Abundance="count", "Basal Area"="size", "Proportion of Plots Occupied"="presab"),
                 seedlings=,shseedlings=,shrubs=,vines=base::c(Abundance="count","Proportion of Plots Occupied"="presab"),
                 herbs=base::c("Percent Cover"="size","Proportion of Plots Occupied"="presab"),
                 cwd=base::c("Volume"="size")
    )
  })
  
  output$densValControl<-shiny::renderUI({
    shiny::selectInput(inputId="densvalues", label="Data to Graph:", choices=DensValuesUse())
  })
  
  #### Species Control (top species vs list) for density plots ####
  densSpecList<-shiny::reactive({
    shiny::req(input$densPark, input$densGroup)
    shiny::req(input$densPark %in% base::names(VEGDATA))
    SpecTemp<-base::unique(NPSForVeg::getPlants(object=VEGDATA[[input$densPark]], group=input$densGroup, years=densYears(),common=F)$Latin_Name)
    SpecNames <- fmt_common(base::tryCatch( NPSForVeg::getPlantNames(object=VEGDATA[[input$densPark]], names=SpecTemp, in.style="Latin", 
                                                                     out.style=base::ifelse(input$densCommon,"common","Latin")), error = function(e) SpecTemp))
    base::names(SpecTemp)<-SpecNames  
    SpecTemp<-SpecTemp[order(base::names(SpecTemp))]
  })
  
  ### create reactive species count ###
  densSpeciesCount <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, densYears())
    veg <- VEGDATA[[input$densPark]]
    shiny::req(veg)
    out <- NPSForVeg::getPlants(object = veg, group = input$densGroup, years = densYears(), common = FALSE)
    base::length(base::unique(out$Latin_Name))})
  
  output$densSpeciesControl<-shiny::renderUI({
    base::switch(input$densSpeciesType,
                 Common= {max_sp <- densSpeciesCount()
                 htmltools::tags$div(title="Select the maximum number of species to plot", 
                                             shiny::sliderInput(inputId="densTop",label="Maximum number of species to plot (in order of mean value):",
                                                                min = 1, max = base::max(1, max_sp, na.rm = TRUE), value = base::min(5, max_sp), step = 1, ticks = FALSE))},
                 Pick= if(base::is.null(input$densPark) || base::nchar(input$densPark)==0) { base::return() }
                 else{
                   htmltools::tags$div(title="Click here to pick the species you want to graph",
                                       shiny::selectizeInput(inputId="densSpecies", label="Select one or more species", choices=densSpecList(),
                                                             multiple=TRUE, selected = input$densSpecies, options = base::list(placeholder='Select species to display',
                                                                                                                               plugins = base::list("remove_button"))
                                       ))
                 }
    )
  })
  
  ### keep species selection when toggling between common/latin names ###
  shiny::observeEvent(input$densCommon, {
    shiny::req(input$densSpeciesType == "Pick")
    current <- shiny::isolate(input$densSpecies)
    shiny::updateSelectizeInput(
      session, "densSpecies",
      choices  = densSpecList(),
      selected = current)
  })
  
  #### Control for comparison ####
  
  #### Compare Years ####
  compYears<-shiny::reactive({
    shiny::req(input$compCycles)
    (DATACYCLES %>% dplyr::filter(Cycle==input$compCycles) %>% dplyr::pull(YearStart)) : 
      (DATACYCLES %>% dplyr::filter(Cycle==input$compCycles) %>% dplyr::pull(YearEnd))
  })
  
  output$CompareSelect<-shiny::renderUI({
    base::switch(input$CompareType, 
                 None=,base::return(),
                 Park= htmltools::tags$div(title= "Select a second park",
                                           shiny::selectizeInput(inputId="ComparePark",choices=c("All Parks" = "ALL", PARKLIST), label="Park:",
                                                                 options = base::list(placeholder='Select a park',onInitialize = base::I('function() { this.setValue(""); }') ))),
                 "Growth Stage"=htmltools::tags$div(title="Select an additional growth stage",
                                                    shiny::selectizeInput(inputId="CompareGroup", label="Growth Stage:", 
                                                                          choices=base::switch(input$densGroup,
                                                                                               trees=,saplings=,seedlings=base::c(Trees="trees", Saplings="saplings", "Tree Seedlings"="seedlings"),
                                                                                               shrubs=, shseedlings=base::c(Shrubs="shrubs", "Shrub Seedlings"="shseedlings"),
                                                                                               vines=,herbs=base::c('Only one growth stage monitored.'=NA)))),
                 Time=htmltools::tags$div(title= "Select a second range of years",
                                          shiny::selectInput(inputId="compCycles", label="Display data from years:", 
                                                             choices=base::rev(stats::setNames(base::as.character(DATACYCLES$Cycle), base::paste0(DATACYCLES$Name,":",
                                                                                                                                                  DATACYCLES$YearStart,"-",DATACYCLES$YearEnd))))))
  })
  
  #### This is currently disabled while the output of dens() for all 0s is reconsidered
  #### Need Compare species to keep the number of species to display to accepted number ####
  # CompareSpecies<-shiny::reactive({
  #   shiny::req(input$CompareType)
  #     base::switch(input$densSpeciesType,
  #       Common=NPSForVeg::getPlantNames( object=VEGDATA[[input$densPark]], out.style="Latin", 
  #               in.style="Latin",
  #               names= base::as.character(dens(object=VEGDATA[[input$densPark]], group=input$densGroup, years=densYears(),
  #                     values=input$densvalues, Total=F, common=F) %>% dplyr::arrange(dplyr::desc(Mean)) %>% dplyr::slice(1:input$densTop) %>% dplyr::pull(Latin_Name))
  #         ),
  #       Pick=input$densSpecies,
  #       All=NA
  #     )
  # })
  
  #### Guard: only certain groups support Growth Stage comparison ####
  growthStageGuard <- shiny::reactive({
    shiny::req(input$CompareType, input$densGroup)
    shiny::validate(shiny::need(base::ifelse(
      base::identical(input$CompareType, "Growth Stage"),
      input$densGroup %in% base::c("trees","saplings","seedlings","shrubs","shseedlings"),
      TRUE),
      "Only one growth stage is monitored for this plant type; 'Growth Stage' comparison is not available. Try 'Park' or 'Time' comparisons or select a different plant type."))
    base::return(TRUE)
  })
  
  #### make compare and labels arguments for densplot() ####
  
  DensCompare<-shiny::reactive({
    shiny::req(input$CompareType, input$densPark, input$densvalues)
    shiny::req(growthStageGuard())
    base::switch(input$CompareType,
                 None=base::return(NA),
                 Park = {
                   if (base::is.null(input$ComparePark) || !nzchar(input$ComparePark)) base::return(NA)
                   if (base::identical(input$ComparePark, "ALL")) {
                     base::return(list(
                       object = NULL,
                       park   = "ALL",
                       group  = input$densGroup,
                       years  = densYears(),
                       values = input$densvalues,
                       common = input$densCommon,
                       area   = if (input$densvalues == "size") "ha" else "plot"))}
                   base::return(list(
                     object = VEGDATA[[input$ComparePark]],
                     park   = input$ComparePark,
                     group  = input$densGroup,
                     years  = densYears(),
                     values = input$densvalues,
                     common = input$densCommon,
                     area   = if (input$densvalues == "size") "ha" else "plot"))},
                 "Growth Stage"=base::return(base::list(object=VEGDATA[[input$densPark]], group=input$CompareGroup, years=densYears(),
                                                        values=input$densvalues,
                                                        common=input$densCommon,area=if(input$densvalues=="size") "ha" else "plot")),
                 Time=base::return(base::list(object=VEGDATA[[input$densPark]], group=input$densGroup, years=compYears(),
                                              values=input$densvalues, 
                                              common=input$densCommon,area=if(input$densvalues=="size") "ha" else "plot")))
  })
  
  DENSLABELDATA<-base::data.frame(Name=base::c("trees","saplings","seedlings","shrubs","shseedlings","herbs","vines"), Label=base::c("Trees","Saplings","Tree Seedlings", "Shrubs","Shrub Seedlings","Understory Plants","Vines in Trees"), stringsAsFactors=FALSE)
  
  #### Y axis labels for density plot ####
  densYlabel<-shiny::reactive({
    base::switch(input$densvalues,
                 count=base::switch(input$densGroup,
                                    trees="Trees / ha",
                                    saplings="Saplings / ha",
                                    seedlings="Tree seedlings / ha",
                                    shrubs="Shrubs / ha",
                                    shseedlings="Shrub seedlings / ha",
                                    vines="Vines on Trees / ha"
                 ),
                 size=base::switch(input$densGroup,
                                   trees=,saplings="Basal area (m\u00B2) / ha",
                                   herbs="Percent Cover",
                                   cwd='(m\u00B3) / ha'
                 ),
                 presab="Proportion of Plots Occupied"
    )
  })
  
  #### Title for density plot ####
  densTitleGroup<-shiny::reactive({
    base::switch(input$densGroup,
                 trees="Tree",
                 saplings="Sapling",
                 seedlings="Tree Seedling",
                 shrubs="Shrub",
                 shseedlings="Shrub Seedling",
                 herbs="Understory Plant",
                 vines="Vines on Trees",
                 cwd='Coarse Woody Debris'
    )  
  })
  
  compareTitleGroup<-shiny::reactive({
    base::switch(input$CompareGroup,
                 trees="Tree",
                 saplings="Sapling",
                 seedlings="Tree Seedling",
                 shrubs="Shrub",
                 shseedlings="Shrub Seedling",
                 herbs="Understory Plant",
                 vines="Vines on Trees",
                 cwd='Coarse Woody Debris'
    )  
  })
  
  densTitleValues<-shiny::reactive({
    base::switch(input$densvalues,
                 count="Abundance",
                 size=base::switch(input$densGroup,
                                   trees=,saplings="Basal Area",
                                   herbs="Percent Cover"
                 ),
                 presab="Proportion of Plots Occupied"
    )
  })
  
  #### redefined later in script ####
  #DensTitle<-shiny::reactive({
  #  base::switch(input$CompareType,
  #               None=  base::return(base::paste(NPSForVeg::getNames(VEGDATA[[input$densPark]],"long"),":",densTitleGroup(),densTitleValues(), 
  #                                               base::paste0(base::as.character(base::min(densYears())),"-",base::as.character(base::max(densYears()))) )),
  #               Park=base::return(base::paste(NPSForVeg::getNames(VEGDATA[[input$densPark]],"long"),"vs.",NPSForVeg::getNames(VEGDATA[[input$ComparePark]],"long"),":",
  #                                             densTitleGroup(),densTitleValues(), 
  #                                             base::paste0(base::as.character(base::min(densYears())),"-",base::as.character(base::max(densYears()))) )),
  #               "Growth Stage"= base::return(base::paste(NPSForVeg::getNames(VEGDATA[[input$densPark]],"long"),":",densTitleGroup(),"vs.",
  #                                                        compareTitleGroup(), densTitleValues(), 
  #                                                        base::paste0(base::as.character(base::min(densYears())),"-",base::as.character(base::max(densYears()))) )),
  #               Time=base::return(base::paste(NPSForVeg::getNames(VEGDATA[[input$densPark]],"long"),":",densTitleGroup(),densTitleValues(), 
  #                                             base::paste0(base::as.character(base::min(densYears())),"-",base::as.character(base::max(densYears()))),"vs.",
  #                                             base::paste0(base::as.character(base::min(compYears())),"-",base::as.character(base::max(compYears()))) ))
  #  )
  #})
  
  
  #### All arguments for densityPlot ####
  #### no longer being used after switching to plotly ####
  #  DensPlotArgs<-shiny::reactive({
  #    base::list(
  #      object=VEGDATA[[input$densPark]],
  #      densargs=base::list(
  #        group=input$densGroup,
  #        years=densYears(),
  #        values=input$densvalues,
  #        common=input$densCommon,
  #        species=base::switch(input$densSpeciesType,
  #                             Pick= {species=input$densSpecies},
  #                             Common=  {species=NA},
  #                             All= {species=NA}
  #        ),
  #        area=if(input$densvalues=="size") "ha" else "plot" 
  #      ),
  #      compare=base::list(DensCompare()),
  #     labels=DensLabels(),
  #      top=base::switch(input$densSpeciesType,
  #                       Common={top=input$densTop},
  #                       Pick={top=NA},
  #                       All = {top=0}
  #      ),
  #      Total=if(input$densSpeciesType=="All"){Total=T} else {Total=F} ,
  #      col=if(input$CompareType=="None"){input$densBaseColor} else{base::c(input$densBaseColor,input$densCompareColor)}, 
  #      ylab=densYlabel(),
  #      main=DensTitle()
  #    )
  #  })
  
  #### Base Density Plot Function ####
  
  # single plot warning label
  densOnePlotWarning <- shiny::reactiveVal(FALSE)
  densOnePlotWarningDismissed <- shiny::reactiveVal(FALSE)
  
  shiny::observeEvent(
    base::list(input$densPark, input$densGroup, input$densCycles, input$densvalues),
    {densOnePlotWarningDismissed(FALSE)})
  
  shiny::observeEvent(input$dismiss_dens_onePlot_warning,
                      {densOnePlotWarningDismissed(TRUE)})
  
  output$densOnePlotWarningGraph <- shiny::renderUI({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    if (densOnePlotWarningDismissed_graph()) base::return(NULL)
    veg <- VEGDATA[[input$densPark]]
    shiny::req(veg)
    n_plots <- base::tryCatch(
      base::nrow(NPSForVeg::getPlots(veg, years = densYears(), type = "all")),
      error = function(e) NA_integer_)
    if (base::is.na(n_plots) || n_plots >= 2) base::return(NULL)
    htmltools::tags$div(style = "padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
                        "Warning: Only one monitoring plot exists for this park and cycle. Confidence intervals cannot be estimated.",
                        htmltools::tags$button("\u00d7", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                                               onclick = "Shiny.setInputValue('dismiss_dens_onePlot_warning_graph', Math.random())"))})
  
  output$densOnePlotWarningTable <- shiny::renderUI({shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    if (densOnePlotWarningDismissed_table()) base::return(NULL)
    veg <- VEGDATA[[input$densPark]]
    shiny::req(veg)
    n_plots <- base::tryCatch(base::nrow(NPSForVeg::getPlots(veg, years = densYears(), type = "all")), error = function(e) NA_integer_)
    if (base::is.na(n_plots) || n_plots >= 2) base::return(NULL)
    htmltools::tags$div(style = "padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
                        "Warning: Only one monitoring plot exists for this park and cycle. Confidence intervals cannot be estimated.",
                        htmltools::tags$button("\u00d7", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                                               onclick = "Shiny.setInputValue('dismiss_dens_onePlot_warning_table', Math.random())"))})
  
  densOnePlotWarningDismissed_graph <- shiny::reactiveVal(FALSE)
  densOnePlotWarningDismissed_table <- shiny::reactiveVal(FALSE)
  
  shiny::observeEvent(base::list(input$densPark, input$densGroup, input$densCycles, input$densvalues),
    {densOnePlotWarningDismissed_graph(FALSE)
     densOnePlotWarningDismissed_table(FALSE)})
  
  shiny::observeEvent(input$dismiss_dens_onePlot_warning_graph, {densOnePlotWarningDismissed_graph(TRUE)})
  shiny::observeEvent(input$dismiss_dens_onePlot_warning_table, {densOnePlotWarningDismissed_table(TRUE)})
  
  ##############bugfix
  densData <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    veg <- VEGDATA[[input$densPark]]
    shiny::req(veg)
    
    densOnePlotWarning(FALSE)
    
    if (input$densvalues == "size" && input$densGroup %in% base::c("vines", "seedlings", "shseedlings")) {
      shiny::validate(shiny::need(FALSE,
                                  base::paste("Basal area / percent cover is not available for", input$densGroup,
                                        "- try 'Abundance' or 'Proportion of Plots Occupied'.")))
    }
    
    n_plots <- base::tryCatch(
      base::nrow(NPSForVeg::getPlots(veg, years = densYears(), type = "all")),
      error = function(e) NA_integer_)
    
    if (!base::is.na(n_plots) && n_plots < 2) {
      sxs <- base::tryCatch({
        args <- base::list(
          object = veg,
          group  = input$densGroup,
          years  = densYears(),
          values = input$densvalues,
          area   = if (input$densvalues == "size") "ha" else "plot"
        )
        if (input$densGroup %in% base::c("trees", "saplings")) {
          args$status <- "alive"
        }
        base::do.call(NPSForVeg::SiteXSpec, args)
      }, error = function(e) {
        message("SiteXSpec fallback error for ", input$densGroup, ":", conditionMessage(e))
        NULL
      })
      
      species_cols <- base::setdiff(base::names(sxs), base::c("Plot_Name", "Total"))
      result <- base::data.frame(
        Latin_Name = species_cols,
        Mean       = base::as.numeric(sxs[1, species_cols]),
        Lower.95   = NA_real_,
        Upper.95   = NA_real_,
        stringsAsFactors = FALSE
      )
      
      base::return(result)
    }
    
    result <- base::tryCatch(
      NPSForVeg::dens(
        object = veg,
        group  = input$densGroup,
        years  = densYears(),
        values = input$densvalues,
        common = FALSE,
        area   = if (input$densvalues == "size") "ha" else "plot",
        Total  = FALSE),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("size", msg, ignore.case = TRUE)) {
          shiny::validate(shiny::need(FALSE,
                                      base::paste("No size measurement available for this plant group:", input$densGroup)))
        }
        message("densData error (likely sparse data): ", msg)
        NULL
      })
    
    shiny::validate(shiny::need(
      !base::is.null(result) && base::nrow(result) > 0,
      base::paste("No", input$densGroup, "were observed at",
            NPSForVeg::getNames(VEGDATA[[input$densPark]], "long"),
            "during", base::min(densYears()), "-", base::max(densYears()), ".")))
    
    result
  }) ###################################
  
  shiny::observe({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    densData()
  })
  
  #### common names checkbox ####
  species_col <- shiny::reactive(if (base::isTRUE(input$densCommon)) "Common_Name" else "Latin_Name")
  
  ### summary statistics checkbox ###
  text_on <- shiny::reactive(base::isTRUE(input$plotlyText))
  
  #### create base plotting df #####
  densDf <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    raw <- densData()
    shiny::req(!base::is.null(raw), base::nrow(raw) > 0)
    
    if (!("Common_Name" %in% base::names(raw))) {
      raw$Common_Name <- fmt_common(base::tryCatch(
        NPSForVeg::getPlantNames(
          object    = VEGDATA[[input$densPark]],
          names     = raw$Latin_Name,
          in.style  = "Latin",
          out.style = "common"),
        error = function(e) base::rep(NA_character_, base::nrow(raw))))
    }
    
    df <- raw %>%
      dplyr::transmute(
        Species = .data[[species_col()]],
        Latin   = .data$Latin_Name,
        Common  = if ("Common_Name" %in% base::names(raw)) .data$Common_Name else NA_character_,
        Mean    = .data$Mean,
        err_up  = dplyr::case_when(
          input$densvalues == "presab" & .data$Mean >= 1 ~ 0,
          TRUE ~ .data$Upper.95 - .data$Mean),
        err_dn  = dplyr::case_when(
          input$densvalues == "presab" & .data$Mean <= 0 ~ 0,
          TRUE ~ .data$Mean - .data$Lower.95)) %>%
      dplyr::filter(!base::tolower(Species) %in% base::c("total", "all species"))
    
    ### radioButton: Pick ###
    if (base::identical(input$densSpeciesType, "Pick")) {
      shiny::req(input$densSpecies)
      df <- df %>% dplyr::filter(Latin %in% input$densSpecies)}
    
    ### radioButton: Common ###
    if (base::identical(input$densSpeciesType, "Common")) {
      shiny::req(input$densTop)
      df <- df %>% dplyr::arrange(dplyr::desc(Mean)) %>% dplyr::slice(1:input$densTop)}
    
    ### radioButton: All ###
    if (base::identical(input$densSpeciesType, "All")) {
      agg_fun <- if (input$densvalues %in% base::c("count","size")) sum else mean
      df <- df %>%
        dplyr::summarise(
          Species = "All species",
          Mean    = agg_fun(Mean,   na.rm = TRUE),
          err_up  = agg_fun(err_up, na.rm = TRUE),
          err_dn  = agg_fun(err_dn, na.rm = TRUE)) %>%
        dplyr::mutate(LabelOpp = "All species")
    } else {
      df <- df %>% dplyr::mutate(
        LabelOpp = dplyr::case_when(
          base::isTRUE(input$densCommon) ~ Latin,
          !base::isTRUE(input$densCommon) ~ dplyr::coalesce(Common, Latin)))}
    
    df <- df %>%
      dplyr::mutate(.tie = base::seq_along(Species)) %>%   # keeps stable order
      dplyr::arrange(dplyr::desc(Mean), .tie) %>%          # FIX: deterministic ties
      dplyr::mutate(Species = base::factor(Species, levels = Species)) %>%
      dplyr::select(-.tie)
  })
  
  #### create compare plotting df #####
  
  ###bugfix update###########################
  dens_all_parks <- function(VEGDATA, group, years, values, common = FALSE) {
    per_park <- base::lapply(base::names(VEGDATA), function(pk) {
      x <- VEGDATA[[pk]]
      out <- base::tryCatch(
        withCallingHandlers(
          NPSForVeg::dens(
            object = x,
            group  = group,
            years  = years,
            values = values,
            common = common,
            area   = if (values == "size") "ha" else "plot",
            Total  = FALSE),
          warning = function(w) {
            # suppress NaN/glm warnings that don't prevent a result
            if (grepl("NaN|theta|qf", conditionMessage(w), ignore.case = TRUE)) {
              invokeRestart("muffleWarning")
            }
          }),
        error = function(e) {
          message("dens_all_parks skipping park '", pk, "': ", conditionMessage(e))
          NULL
        })
      
      if (base::is.null(out) || base::nrow(out) == 0) base::return(NULL)
      
      n_plots <- base::tryCatch(
        base::nrow(NPSForVeg::getPlots(x, years = years, type = "all")),
        error = function(e) 1L)
      
      out$.n_plots <- n_plots
      out
    })
    
    per_park <- base::Filter(base::Negate(base::is.null), per_park)
    if (!base::length(per_park)) base::return(NULL)
    
    combined <- dplyr::bind_rows(per_park)
    
    combined %>%
      dplyr::group_by(Latin_Name) %>%
      dplyr::summarise(
        Mean     = stats::weighted.mean(Mean,     .n_plots, na.rm = TRUE),
        Lower.95 = stats::weighted.mean(Lower.95, .n_plots, na.rm = TRUE),
        Upper.95 = stats::weighted.mean(Upper.95, .n_plots, na.rm = TRUE),
        .groups  = "drop")
  }##########################################
  
  compareDf <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    cmp <- DensCompare()
    if (base::is.null(cmp) || (base::is.atomic(cmp) && base::is.na(cmp))) base::return(NULL)
    shiny::req(base::is.list(cmp), cmp$group, cmp$years, cmp$values)
    ###bugfix update#####################################
    if (base::identical(cmp$park, "ALL")) {
      raw <- dens_all_parks(
        VEGDATA = VEGDATA,
        group   = cmp$group,
        years   = cmp$years,
        values  = cmp$values
      )
      if (base::is.null(raw) || base::nrow(raw) == 0) base::return(NULL)
      
      # Align to base species — fill missing with zeros
      base_species <- densData()$Latin_Name
      missing_species <- base::setdiff(base_species, raw$Latin_Name)
      
      if (base::length(missing_species) > 0) {
        empty_rows <- base::data.frame(
          Latin_Name = missing_species,
          Mean       = 0,
          Lower.95   = 0,
          Upper.95   = 0,
          stringsAsFactors = FALSE
        )
        extra_cols <- base::setdiff(names(raw), base::names(empty_rows))
        for (col in extra_cols) empty_rows[[col]] <- NA
        raw <- dplyr::bind_rows(raw, empty_rows[, base::names(raw), drop = FALSE])
      }
      
      # Drop Park column if present (added by old version, not by new)
      raw <- raw %>% dplyr::select(-dplyr::any_of("Park"))
      ##############################################
      
    } else {
      shiny::req(cmp$object)
      raw <- NPSForVeg::dens(
        object = cmp$object,
        group  = cmp$group,
        years  = cmp$years,
        values = cmp$values,
        common = FALSE,
        area   = cmp$area,
        Total  = FALSE)
      if (base::is.null(raw) || base::nrow(raw) == 0) base::return(NULL)}
    
    ### add common names if requested ###
    
    ###bugfix update: Replace the name_obj line and getPlantNames call in compareDf#########################
    name_obj <- VEGDATA[[input$densPark]] %||% VEGDATA[[PARKLIST[1]]]
    
    if (!("Common_Name" %in% base::names(raw))) {
      raw$Common_Name <- fmt_common(base::tryCatch({
        # Filter to only names that exist in the object before looking up
        known <- raw$Latin_Name[raw$Latin_Name %in% 
                                  NPSForVeg::getPlants(name_obj, group = cmp$group)$Latin_Name]
        
        result <- base::rep(NA_character_, base::nrow(raw))
        if (base::length(known) > 0) {
          looked_up <- NPSForVeg::getPlantNames(
            object    = name_obj,
            names     = known,
            in.style  = "Latin",
            out.style = "common"
          )
          result[raw$Latin_Name %in% known] <- looked_up
        }
        result
      }, error = function(e) {
        base::rep(NA_character_, base::nrow(raw))
      }))
    }
    ###########################################################
    
    species_col_val <- species_col()
    
    df <- raw %>%
      dplyr::transmute(
        Species = .data[[species_col_val]],
        Latin   = Latin_Name,
        Common  = Common_Name,
        Mean    = Mean,
        err_up  = Upper.95 - Mean,
        err_dn  = Mean - Lower.95
      ) %>%
      dplyr::filter(!base::tolower(Species) %in% base::c("total", "all species"))
    
    ###selection rules
    # Pick
    if (base::identical(input$densSpeciesType, "Pick")) {
      shiny::req(input$densSpecies)
      df <- df %>% dplyr::filter(Latin %in% input$densSpecies)}
    
    # Common = top N from BASE (important!)
    if (base::identical(input$densSpeciesType, "Common")) {
      base_species <- densDf()$Species
      df <- df %>% dplyr::filter(Species %in% densDf()$Species)}
    
    # All species combined
    if (base::identical(input$densSpeciesType, "All")) {
      agg_fun <- if (cmp$values %in% base::c("count","size")) sum else mean
      df <- df %>%
        dplyr::summarise(
          Species = "All species",
          Mean    = agg_fun(Mean,   na.rm = TRUE),
          err_up  = agg_fun(err_up, na.rm = TRUE),
          err_dn  = agg_fun(err_dn, na.rm = TRUE)) %>%
        dplyr::mutate(LabelOpp = "All species")} else {
          df <- df %>% dplyr::mutate(
            LabelOpp = dplyr::case_when(
              base::isTRUE(input$densCommon) ~ Latin,
              TRUE                     ~ dplyr::coalesce(Common, Latin)))}
    
    # zero-fill base species missing from compare
    if (!base::identical(input$densSpeciesType, "All")) {
      base_latin <- densDf()$Latin
      missing <- base::setdiff(base_latin, df$Latin)
      if (base::length(missing) > 0) {
        zero_rows <- base::data.frame(
          Species  = base::as.character(densDf()$Species[densDf()$Latin %in% missing]),
          Latin = missing,
          Common  = NA_character_,
          Mean = 0,
          err_up = NA_real_,
          err_dn = NA_real_,
          LabelOpp = base::as.character(densDf()$Species[densDf()$Latin %in% missing]),
          stringsAsFactors = FALSE)
        df <- dplyr::bind_rows(df, zero_rows)}
      df$Species <- base::factor(df$Species, levels = base::union(
        base::levels(densDf()$Species),
        base::as.character(df$Species)))}
    
    df
  })
  
  #### This is currently disabled while the output of dens() for all 0s is reconsidered
  #### Need Compare species to keep the number of species to display to accepted number ####
  # CompareSpecies<-shiny::reactive({
  #   shiny::req(input$CompareType)
  #     base::switch(input$densSpeciesType,
  #       Common=NPSForVeg::getPlantNames( object=VEGDATA[[input$densPark]], out.style="Latin", 
  #               in.style="Latin",
  #               names= base::as.character(dens(object=VEGDATA[[input$densPark]], group=input$densGroup, years=densYears(),
  #                     values=input$densvalues, Total=F, common=F) %>% dplyr::arrange(dplyr::desc(Mean)) %>% dplyr::slice(1:input$densTop) %>% dplyr::pull(Latin_Name))
  #         ),
  #       Pick=input$densSpecies,
  #       All=NA
  #     )
  # })
  
  
  #### make compare and labels arguments for densplot() ####
  
  DensTitle <- shiny::reactive({
    shiny::req(input$densPark)
    base_obj <- VEGDATA[[input$densPark]]; shiny::req(base_obj)
    
    base_name <- NPSForVeg::getNames(base_obj, "long")
    period1   <- base::paste0(base::as.character(base::min(densYears())), "-", base::as.character(base::max(densYears())))
    grp_title <- densTitleGroup()
    val_title <- densTitleValues()
    
    base::switch(input$CompareType,
                 None = base::return(base::paste(base_name, ":", grp_title, val_title, period1)),
                 Park = {if (base::is.null(input$ComparePark) || !base::nzchar(input$ComparePark)) {
                   base::paste(base_name, ":", grp_title, val_title, period1)
                   } else if (base::identical(input$ComparePark, "ALL")) {
                     base::paste(base_name, "vs. All Parks:", grp_title, val_title, period1)
                   } else {cmp_obj  <- VEGDATA[[input$ComparePark]]
                   cmp_name <- if (!base::is.null(cmp_obj)) {
                     NPSForVeg::getNames(cmp_obj, "long")
                     } else {input$ComparePark}
                     base::paste(base_name, "vs.", cmp_name, ":", grp_title, val_title, period1)}},
                 "Growth Stage" = base::return(
                   {stage_order <- base::c("seedlings", "saplings", "trees", "shseedlings", "shrubs")
                    base_idx <- base::match(input$densGroup, stage_order)
                    cmp_idx  <- base::match(input$CompareGroup, stage_order)
                    if (base_idx <= cmp_idx) {base::paste(base_name, ":", grp_title, "vs.", compareTitleGroup(), val_title, period1)
                      } else {base::paste(base_name, ":", compareTitleGroup(), "vs.", grp_title, val_title, period1)}}),
                 Time = base::return(
                   {base_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$densCycles]
                    cmp_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$compCycles]
                    if (base_yr <= cmp_yr) {base::paste(base_name, ":", grp_title, val_title, period1, "vs.",
                                                        base::paste0(base::as.character(base::min(compYears())), "-",
                                                                     base::as.character(base::max(compYears()))))
                      } else {base::paste(base_name, ":", grp_title, val_title, base::paste0(base::as.character(base::min(compYears())), "-",
                                                                                             base::as.character(base::max(compYears()))), "vs.", period1)}}))})
  
  ### generate dens warning label ###
  densMissingWarningData <- shiny::reactive({
    base_df <- densDf()
    cmp_df <- compareDf()
    
    shiny::req(base_df, cmp_df)
    
    merged <- dplyr::left_join(
      base_df %>% dplyr::select(Latin, Species),
      cmp_df  %>% dplyr::select(Latin, Mean),
      by = "Latin"
    )
    
    missing <- merged %>%
      dplyr::filter(base::is.na(Mean) | Mean == 0)
    
    if (base::nrow(missing) == 0) base::return(NULL)
    
    base::list(
      n = base::nrow(missing),
      base_name = park_long_name(input$densPark),
      cmp_name = if (input$CompareType == "Park") {
        if (base::identical(input$ComparePark, "ALL")) {
          "All Parks"
        } else {
          park_long_name(input$ComparePark)
        }
      } else if (input$CompareType == "Time") {
        "comparison period"
      } else if (input$CompareType == "Growth Stage") {
        "comparison growth stage"
      } else {
        "comparison dataset"
      }
    )
  })
  
  densWarningDismissed <- shiny::reactiveVal(FALSE)
  
  shiny::observeEvent(
    base::list(input$densPark, input$densGroup, input$densCycles, input$densvalues, input$CompareType, input$ComparePark, input$compCycles, input$CompareGroup),
    {densWarningDismissed(FALSE)})
  
  shiny::observeEvent(input$dismiss_dens_warning,
    {densWarningDismissed(TRUE)})
  
  park_long_name <- function(park_key) {
    if (base::is.null(park_key) || !base::nzchar(park_key)) base::return(park_key)
    obj <- VEGDATA[[park_key]]
    if (!base::is.null(obj)) {NPSForVeg::getNames(obj, "long")
    } else {park_key}}
  
  output$densMissingWarningGraph <- shiny::renderUI({
    
    info <- densMissingWarningData()
    if (densWarningDismissed()) base::return(NULL)
    shiny::req(info)
    
    n <- info$n
    verb <- if (n == 1) "is" else "are"
    
    msg <- base::switch(input$CompareType, 
                        Park = base::paste0("Warning: ", n, " species in ", info$base_name, " ", verb, " not present in ", info$cmp_name, " (shown as 0 on the figure)."),
                        "Growth Stage" = {base_group <- base::switch(input$densGroup, trees = "tree", saplings = "sapling", seedlings = "seedling", 
                                                               shrubs = "shrub", shseedlings = "shrub seedling", input$densGroup)
                                          cmp_stage <- base::switch(input$CompareGroup, trees = "trees", saplings = "saplings", seedlings = "tree seedlings", 
                                                              shrubs = "shrubs", shseedlings = "shrub seedlings", input$CompareGroup)
                                          base::paste0("Warning: ", n, " ", base_group, " species are not present as ", cmp_stage, " in ", info$base_name, " (shown as 0 on the figure).")},
                        Time = {base_cycle <- DATACYCLES$Name[DATACYCLES$Cycle == input$densCycles]
                                base_years <- base::paste0(DATACYCLES$YearStart[DATACYCLES$Cycle == input$densCycles], "-", DATACYCLES$YearEnd[DATACYCLES$Cycle == input$densCycles])
                                cmp_cycle <- DATACYCLES$Name[DATACYCLES$Cycle == input$compCycles]
                                cmp_years <- base::paste0(DATACYCLES$YearStart[DATACYCLES$Cycle == input$compCycles], "-", DATACYCLES$YearEnd[DATACYCLES$Cycle == input$compCycles])
                                base::paste0("Warning: ", n, " species in ", info$base_name, " present in ", base_cycle, " (", base_years, ")",
                                             " ", verb, " not present in ", cmp_cycle, " (", cmp_years, ")", " (shown as 0 on the figure).")})
  htmltools::tags$div(
      style = " padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
      msg, 
      htmltools::tags$button("×", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                             onclick = "Shiny.setInputValue('dismiss_dens_warning', Math.random())"))})
  
  output$DensPlotly <- plotly::renderPlotly({
    
    shiny::validate(shiny::need(
      !base::is.null(input$densPark) && base::nzchar(input$densPark),
      "Select a park to display the graph."))
    
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    
    df <- densDf()
    df_cmp <- compareDf()
    
    ### for scatter plot plotly ###  
    #offset compare data from base data on plot
    #  species_levels <- base::unique(base::c(df$Species, if(!base::is.null(df_cmp)) df_cmp$Species))
    #  species_idx <- stats::setNames(base::seq_along(species_levels), species_levels)
    
    #  offset <- 0.25
    
    #  df$y_base <- species_idx[df$Species]
    #  if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) {
    #    df_cmp$y_cmp <- species_idx[df_cmp$Species] - offset}
    
    show_compare <- input$CompareType != "None"
    
    species_levels <- if (!base::is.null(df) && base::nrow(df) > 0) {
      base::as.character(df$Species)} else {
        base::unique(base::c(df$Species, if (!base::is.null(df_cmp)) df_cmp$Species))}
    species_levels <- base::unique(species_levels)
    
    if (!base::is.null(df) && base::nrow(df) > 0) {
      df$Species <- base::factor(df$Species, levels = species_levels)}
    if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) {
      df_cmp$Species <- base::factor(df_cmp$Species, levels = species_levels)}
    
    #make labels readable
    park_long_name <- function(park_key) {
      if (base::is.null(park_key) || !nzchar(park_key)) base::return(park_key)
      obj <- VEGDATA[[park_key]]
      if (!base::is.null(obj)) {return(NPSForVeg::getNames(obj, "long"))}}
    
    cycle_long_name <- function(cycle_code) {
      if (base::is.null(cycle_code) || !nzchar(cycle_code)) base::return(cycle_code)
      row <- DATACYCLES[DATACYCLES$Cycle == cycle_code, ]
      if (base::nrow(row) == 1L) {
        base::paste0(row$Name, ":", row$YearStart, "-", row$YearEnd)} else {
          base::as.character(cycle_code)}}
    
    species_group_long <- stats::setNames(DENSLABELDATA$Label, DENSLABELDATA$Name)
    group_long_name <- function(group_key) {
      val <- species_group_long[group_key]
      if (base::length(val) == 0 || base::is.na(val)) group_key else val}
    
    park_display         <- park_long_name(input$densPark)
    cmp_park_display     <- park_long_name(input$ComparePark)
    cycle_display_base   <- cycle_long_name(input$densCycles)
    cycle_display_cmp    <- cycle_long_name(input$compCycles)
    group_display_base   <- group_long_name(input$densGroup)
    group_display_cmp    <- group_long_name(input$CompareGroup)
    
    #wire legend to selections
    base_legend <- base::paste(
      if (input$CompareType == "Park")
        base::paste0("<b>Park: ", park_display, "</b>")
      else base::paste0("Park: ", park_display),
      if (input$CompareType == "Time")
        base::paste0("| <b>",cycle_display_base, "</b>")
      else base::paste0("| ",cycle_display_base),
      if (input$CompareType == "Growth Stage")
        base::paste0("| <b>Plant Type: ", group_display_base, "</b>")
      else base::paste0("| Plant Type: ", group_display_base))
    
    cmp_legend <- base::switch(input$CompareType,
                               None = "Compare",
                               Park = {if (base::identical(input$ComparePark, "ALL")) {
                                 base::paste("<b>Park: All Parks</b>",
                                             "| ", cycle_display_base,
                                             "| Plant Type:", group_display_base)} else {
                                               base::paste(
                                                 "<b>Park:", cmp_park_display, "</b>",
                                                 "| ", cycle_display_base,
                                                 "| Plant Type:", group_display_base)}},
                               "Growth Stage" = base::paste(
                                 "Park:",park_display, 
                                 "| ", cycle_display_base,
                                 "| <b> Growth Stage:",group_display_cmp, "</b>"),
                               Time = base::paste(
                                 "Park:",park_display, 
                                 "| <b>",cycle_display_cmp, "</b>",
                                 "| Plant Type:",group_display_base))
    
    # graphing colors
    densBaseColor <- shiny::reactive(toHex(pickColor(input$densBaseColor, "blue")))
    densCmpColor  <- shiny::reactive(toHex(pickColor(input$densCompareColor, "red")))
    densFontSize  <- shiny::reactive(if (!base::is.null(input$densFontSize)) input$densFontSize else 12)
    
    # error bar colors
    darken <- function(hex, factor = 0.6) {rgb <- grDevices::col2rgb(hex)
    grDevices::rgb(rgb[1] * factor, rgb[2] * factor, rgb[3] * factor, maxColorValue = 255)}
    
    ### for bar chart plotly ###  
    # n_species <- base::length(species_levels)
    # bars_per_species <- if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) 2 else 1
    # 
    # 
    # row_px <- if (n_species == 1) 200 else 100 * bars_per_species
    # 
    # top_pad_px    <- 50
    # bottom_pad_px <- 50
    # fig_height <- top_pad_px + n_species * row_px + bottom_pad_px
    
    ### bind into one df
    df$group     <- base_legend
    df$Species   <- base::factor(df$Species, levels = species_levels)
    
    if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) {
      df_cmp$group   <- cmp_legend
      df_cmp$Species <- base::factor(df_cmp$Species, levels = species_levels)
      df <- dplyr::bind_rows(df, df_cmp)}
    
    #plot in plotly
    p <- plotly::plot_ly()
    
    grp_order <- base::switch(input$CompareType,
      Time = {base_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$densCycles]
              cmp_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$compCycles]
              if (base_yr <= cmp_yr) base::c(cmp_legend, base_legend) else base::c(base_legend, cmp_legend)},
      "Growth Stage" = {stage_order <- base::c("seedlings", "saplings", "trees", "shseedlings", "shrubs")
             base_idx <- base::match(input$densGroup, stage_order)
             cmp_idx <- base::match(input$CompareGroup, stage_order)
             if (base_idx <= cmp_idx) base::c(cmp_legend, base_legend) else base::c(base_legend, cmp_legend)},
     base::c(cmp_legend, base_legend))

    for (grp in grp_order) {df_all <- df[df$group == grp, ]
      bar_color <- if (grp == base_legend) densBaseColor() else densCmpColor()
      err_color <- darken(bar_color)
      
      p <- p %>% plotly::add_trace(
      data = df_all,
      y = ~Species,
      x = ~Mean,
      type = "bar",
      orientation = "h",
      #mode = "markers", ##for scatter plot
      name = grp,   
      showlegend = TRUE,        
      marker = base::list(color = bar_color),
      hovertext = ~base::sprintf(
        "Species: %s<br>Mean: %.2f<br>Lower 95%%: %.2f<br>Upper 95%%: %.2f",
        LabelOpp,
        Mean,
        Mean - err_dn,
        Mean + err_up),
      text = if (text_on()) {
        ~base::sprintf(
          "Species: %s<br>Mean: %.2f<br>Lower 95%%: %.2f<br>Upper 95%%: %.2f",
          LabelOpp,
          Mean,
          Mean - err_dn,
          Mean + err_up)} else {""},
      hoverinfo = if (text_on()) "none" else "text",
      error_x = base::list(
        thickness = input$densErrorThickness,
        type = "data",
        array = df_all$err_up,
        arrayminus = df_all$err_dn,
        color = err_color),
      legendgroup = "dens")}
    
    p <- p %>% plotly::layout(
      barmode = "group",
      showlegend = TRUE,
      legend = base::list(
        traceorder = "reversed", 
        font = base::list(size = densFontSize() + 3),
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = 1, yanchor = "bottom"),
      title = base::list(
        text = DensTitle(),
        font = base::list(size = densFontSize() + 10),
        y = 1,
        yanchor = "top",
        pad = base::list(t = 20),
        automargin = TRUE,
        xref = "paper",
        x = 0.5,
        xanchor = "center"),
      xaxis = base::list(
        showgrid = TRUE,
        title = base::list(
          text = densYlabel(),
          font = base::list(size = densFontSize()),
          standoff = 20)),
      yaxis = base::list(
        type = "category", categoryorder = "array", categoryarray = rev(species_levels),
        showline = TRUE,
        title = base::list(
          text = "Species",
          font = base::list(size = densFontSize()),
          standoff = 15),
        ticks = "outside",
        ticklabelposition = "outside"),
      margin = base::list(t = 50 + densFontSize() * 3, l = 140, r = 40),
      #height = fig_height,
      font = base::list(size = densFontSize()),
      autosize = TRUE)
    p
  })

  
  ####### original graphs and file downloads #######
  #tempDensPlot<-shiny::reactive({
  #  if (base::is.null(input$densPark) || base::nchar(input$densPark)==0) {base::return()}
  #    else{
  #      shiny::validate(shiny::need(base::try(
  #        base::do.call(densplot,DensPlotArgs() )),
  #       "There is no data for this combination of choices. The type of plant you selected was not found in the park during those years."
  #       ))
  #      stats::update(base::do.call(densplot, DensPlotArgs()), par.settings=base::list(fontsize=base::list(text=input$densFontSize,
  #                                                                                points=input$densPointSize )))
  #    }
  #})
  #output$DensPlot<-shiny::renderPlot(base::print(tempDensPlot()))
  
  
  ##### jpeg Plot download ####
  #output$densGraphDownload<-shiny::downloadHandler(
  #  filename=function(){base::paste(DensTitle(), ".jpeg", sep="")}, 
  #  content=function (file){
  #    grDevices::jpeg(file,width=15,height=6,units="in",res=300, quality=100)
  #    base::print(tempDensPlot())
  #    grDevices::dev.off()
  #  }
  #)
  
  ##### wmf plot download ####
  #output$densWmfDownload<-shiny::downloadHandler(
  #  filename=function(){base::paste(DensTitle(), ".wmf", sep="")}, 
  #  content=function (file){
  #    grDevices::win.metafile(file,width=15,height=6)
  #    base::print(tempDensPlot())
  #    grDevices::dev.off()
  #  }
  #)
  
  #### Tables Tab ####
  #### Title for table ####
  tempDensTableTitle <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    base_obj <- VEGDATA[[input$densPark]]
    shiny::req(base_obj)
    base_name <- NPSForVeg::getNames(base_obj, "long")
    base_period <- base::paste0(
      base::min(densYears()), "-",
      base::max(densYears()))
    grp_title <- densTitleGroup()
    val_title <- densTitleValues()
    unit_suffix <- base::paste0(" (", densYlabel(), ")")
    
    # no comparison selected
    if (input$CompareType == "None") {
      base::return(base::paste(base_name, ":", grp_title, val_title, base_period, unit_suffix))}
    
    # park comparison selected
    if (input$CompareType == "Park") {
      if (base::is.null(input$ComparePark) || !base::nzchar(input$ComparePark)) {
        base::return(base::paste(base_name, ":", grp_title, val_title, base_period, unit_suffix))}
      
      cmp_name <- if (base::identical(input$ComparePark, "ALL")) {"All Parks"
      } else {cmp_obj <- VEGDATA[[input$ComparePark]]
        if (!base::is.null(cmp_obj)) {NPSForVeg::getNames(cmp_obj, "long")
        } else {input$ComparePark}}
      
      base::return(base::paste(base_name, "vs.", cmp_name, ":", grp_title, val_title, base_period, unit_suffix))}
    
    # growth stage comparison selected
    if (base::identical(input$CompareType, "Growth Stage") && !base::is.null(input$densGroup) && !base::is.null(input$CompareGroup)) {
      stage_order <- base::c("seedlings", "saplings", "trees", "shseedlings", "shrubs")
      base_idx <- base::match(input$densGroup, stage_order)
      cmp_idx  <- base::match(input$CompareGroup, stage_order)
      cmp_title <- compareTitleGroup()
      if (base::is.na(base_idx) || base::is.na(cmp_idx)) {
        base::return(base::paste(base_name, ":", grp_title, "vs.", cmp_title, val_title, base_period, unit_suffix))}
      if (base_idx <= cmp_idx) {
        base::return(base::paste(base_name, ":", grp_title, "vs.", cmp_title, val_title, base_period, unit_suffix))
      } else {
        base::return(base::paste(base_name, ":", cmp_title, "vs.", grp_title, val_title, base_period, unit_suffix))}}

    # time comparison selected
    if (input$CompareType == "Time") {cmp_period <- base::paste0(min(compYears()), "-", base::max(compYears()))
      base_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$densCycles]
      cmp_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$compCycles]
      
      if (base_yr <= cmp_yr) {
        base::return(base::paste(base_name, ":", grp_title, val_title, base_period, "vs", cmp_period, unit_suffix))
      } else {
        base::return(base::paste(base_name, ":", grp_title, val_title, cmp_period, "vs", base_period, unit_suffix))}}
    })
  
  output$densTableTitle <- shiny::renderText({ tempDensTableTitle() })
  
  #### Data for Table ####
  DensTableArgs<-shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    veg <- VEGDATA[[input$densPark]]
    shiny::req(veg)
    base::list(
      object = veg,
      group = input$densGroup,
      years = densYears(),
      values = input$densvalues,
      area = base::ifelse(input$densvalues == "size", "ha", "plot"),
      common = base::isTRUE(input$densCommon))})
  
  #### reactive label for dataset column ####
  densDatasetLabels <- shiny::reactive({
    shiny::req(input$densPark)
    base_obj <- VEGDATA[[input$densPark]]
    
    base_label <- base::switch(input$CompareType,
                         Park = NPSForVeg::getNames(base_obj, "long"),
                         "Growth Stage" = DENSLABELDATA$Label[DENSLABELDATA$Name == input$densGroup],
                         Time = {row <- DATACYCLES[DATACYCLES$Cycle == input$densCycles, ]
                           if (base::nrow(row) == 1L) base::paste0(row$Name, ":", row$YearStart, "-", row$YearEnd)
                           else base::as.character(input$densCycles)},
                         "Base")
    cmp_label <- base::switch(input$CompareType,
                        Park = {if (base::identical(input$ComparePark, "ALL")) "All Parks"
                          else {cmp_obj <- VEGDATA[[input$ComparePark]]
                          if (!base::is.null(cmp_obj)) NPSForVeg::getNames(cmp_obj, "long") else input$ComparePark}},
                        "Growth Stage" = DENSLABELDATA$Label[DENSLABELDATA$Name == input$CompareGroup],
                        Time = {row <- DATACYCLES[DATACYCLES$Cycle == input$compCycles, ]
                          if (base::nrow(row) == 1L) base::paste0(row$Name, ":", row$YearStart, "-", row$YearEnd)
                          else base::as.character(input$compCycles)},
                        "Compare")
    base::list(base = base_label, cmp = cmp_label)})
  
  # table warning label
  output$densMissingWarningTable <- shiny::renderUI({
    
    info <- densMissingWarningData()
    if (densWarningDismissed()) base::return(NULL)
    shiny::req(info)
    
    n <- info$n
    verb <- if (n == 1) "is" else "are"
    
    msg <- base::switch(input$CompareType, 
                        Park = base::paste0("Warning: ", n, " species in ", info$base_name, " ", verb, " not present in ", info$cmp_name, " (shown as 0 on the figure)."),
                        "Growth Stage" = {base_group <- base::switch(input$densGroup, trees = "tree", saplings = "sapling", seedlings = "seedling", 
                                                               shrubs = "shrub", shseedlings = "shrub seedling", input$densGroup)
                        cmp_stage <- base::switch(input$CompareGroup, trees = "trees", saplings = "saplings", seedlings = "tree seedlings", 
                                            shrubs = "shrubs", shseedlings = "shrub seedlings", input$CompareGroup)
                        base::paste0("Warning: ", n, " ", base_group, " species are not present as ", cmp_stage, " in ", info$base_name, " (shown as 0 on the figure).")},
                        Time = {base_cycle <- DATACYCLES$Name[DATACYCLES$Cycle == input$densCycles]
                        base_years <- base::paste0(DATACYCLES$YearStart[DATACYCLES$Cycle == input$densCycles], "-", DATACYCLES$YearEnd[DATACYCLES$Cycle == input$densCycles])
                        cmp_cycle <- DATACYCLES$Name[DATACYCLES$Cycle == input$compCycles]
                        cmp_years <- base::paste0(DATACYCLES$YearStart[DATACYCLES$Cycle == input$compCycles], "-", DATACYCLES$YearEnd[DATACYCLES$Cycle == input$compCycles])
                        base::paste0("Warning: ", n, " species in ", info$base_name, " present in ", base_cycle, " (", base_years, ")", " ", verb, " not present in ", cmp_cycle, 
                                     " (", cmp_years, ")", " (shown as 0 on the figure).")})
    htmltools::tags$div(
      style = " padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
      msg, 
      htmltools::tags$button("×", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                             onclick = "Shiny.setInputValue('dismiss_dens_warning', Math.random())"))})

  #### Make Table ####
  tempDensTable <- shiny::reactive({
    shiny::validate(shiny::need(!base::is.null(input$densPark) && base::nzchar(input$densPark),
                                "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."))
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    raw <- densData()
    shiny::req(!base::is.null(raw), base::nrow(raw) > 0)
    
    if (!("Common_Name" %in% base::names(raw))) {
      raw$Common_Name <- tryCatch(
        NPSForVeg::getPlantNames(
          object = VEGDATA[[input$densPark]],
          names = raw$Latin_Name,
          in.style = "Latin",
          out.style = "common"),
        error = function(e) base::rep(NA_character_, base::nrow(raw)))}
    
    name_col <- if (base::isTRUE(input$densCommon)) "Common_Name" else "Latin_Name"
    
    build_half <- function(df_raw, name_col_arg) {
      df_raw %>%
        dplyr::transmute(
          Latin_Name = Latin_Name,
          Species = fmt_common(.data[[name_col_arg]]),
          Mean = base::as.numeric(Mean),
          Lower95 = base::as.numeric(Lower.95),
          Upper95 = base::as.numeric(Upper.95)) %>%
        dplyr::filter(!base::tolower(Species) %in% base::c("total", "all species"))}
    
    filter_half <- function(tbl) {
      if (base::identical(input$densSpeciesType, "Pick")) {
        shiny::req(input$densSpecies)
        tbl <- tbl %>% dplyr::filter(Latin_Name %in% input$densSpecies)}
      if (base::identical(input$densSpeciesType, "Common")) {
        shiny::req(input$densTop)
        tbl <- tbl %>% dplyr::arrange(dplyr::desc(Mean)) %>% dplyr::slice(1:input$densTop)}
      if (base::identical(input$densSpeciesType, "All")) {
        agg_fun <- if (input$densvalues %in% base::c("count", "size")) sum else mean
        tbl <- tbl %>% dplyr::summarise(
          Latin_Name = "All species",
          Species = "All species",
          Mean = agg_fun(Mean, na.rm = TRUE),
          Lower95 = agg_fun(Lower95, na.rm = TRUE),
          Upper95 = agg_fun(Upper95, na.rm = TRUE))}
      tbl}
    
    # make missing observations 0 means and NA CIs in table 
    fmt <- function(tbl) {
      tbl %>%
        dplyr::rename(`Lower 95% CI` = Lower95, `Upper 95% CI` = Upper95) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(base::c("Lower 95% CI", "Upper 95% CI")),
          ~ dplyr::if_else(base::is.na(Mean) | Mean == 0, NA_real_, .x))) %>%
        # make sure rounding is consistent
        dplyr::mutate(dplyr::across(dplyr::all_of(base::c("Mean", "Lower 95% CI", "Upper 95% CI")),
          ~ base::ifelse(base::is.na(.x), "NA", base::format(base::round(base::as.numeric(.x), 3), nsmall = 3, scientific = FALSE))))}
    
    base_half <- build_half(raw, name_col) %>% filter_half() %>% dplyr::select(-Latin_Name)
    
    # no comparison selected
    if (input$CompareType == "None") {
      base_half <- base_half %>%
        dplyr::mutate(Species = base::factor(Species, levels = base::levels(densDf()$Species))) %>%
        dplyr::arrange(Species)
      base::return(fmt(base_half))}
    
    # comparison dfselected
    df_cmp_raw <- compareDf()
    
    if (base::is.null(df_cmp_raw) || base::nrow(df_cmp_raw) == 0) {
      base_half <- base_half %>%
        dplyr::mutate(Species = base::factor(Species, levels = base::levels(densDf()$Species))) %>%
        dplyr::arrange(Species)
      base::return(fmt(base_half))}
    
      cmp_half <- df_cmp_raw %>%
        dplyr::transmute(
          Species = base::as.character(Species),
          Mean = base::as.numeric(Mean),
          Lower95 = base::as.numeric(Mean) - base::as.numeric(err_dn),
          Upper95 = base::as.numeric(Mean) + base::as.numeric(err_up)) %>%
        dplyr::filter(!base::tolower(Species) %in% base::c("total", "all species"))
    
    # fill 0 if species is absent in compare
    if (!base::identical(input$densSpeciesType, "All")) {
      base_species <- base::as.character(base_half$Species)
      missing <- base::setdiff(base_species, base::as.character(cmp_half$Species))
      if (base::length(missing) > 0) {
        zero_rows <- base::data.frame(
          Species = missing,
          Mean = 0,
          Lower95 = NA_real_,
          Upper95 = NA_real_,
          stringsAsFactors = FALSE)
        cmp_half <- dplyr::bind_rows(cmp_half, zero_rows)}}
    
    # reactive labels
    base_obj   <- VEGDATA[[input$densPark]]
    base_label <- base::switch(input$CompareType, 
                         Park = NPSForVeg::getNames(base_obj, "long"),
                         "Growth Stage" = DENSLABELDATA$Label[DENSLABELDATA$Name == input$densGroup],
                         Time = {row <- DATACYCLES[DATACYCLES$Cycle == input$densCycles, ]
                           if (base::nrow(row) == 1L) base::paste0(row$Name, ":", row$YearStart, "-", row$YearEnd)
                           else base::as.character(input$densCycles)},
                         "Base")
    
    cmp_label <- base::switch(input$CompareType,
                        Park = {if (base::identical(input$ComparePark, "ALL")) "All Parks"
                          else {cmp_obj <- VEGDATA[[input$ComparePark]]
                            if (!base::is.null(cmp_obj)) NPSForVeg::getNames(cmp_obj, "long") else input$ComparePark}},
                        "Growth Stage" = DENSLABELDATA$Label[DENSLABELDATA$Name == input$CompareGroup],
                        Time = {row <- DATACYCLES[DATACYCLES$Cycle == input$compCycles, ]
                          if (base::nrow(row) == 1L) base::paste0(row$Name, ":", row$YearStart, "-", row$YearEnd)
                          else base::as.character(input$compCycles)},
                        "Compare")
    
    # follow the same ordering rules as the plot
    base_on_top <- base::switch(input$CompareType,
                          Park = TRUE,
                          "Growth Stage" = {
                            stage_order <- base::c("seedlings", "saplings", "trees", "shseedlings", "shrubs")
                            base_idx <- base::match(input$densGroup, stage_order)
                            cmp_idx <- base::match(input$CompareGroup, stage_order)
                            base_idx <= cmp_idx},
                          Time = {
                            base_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$densCycles]
                            cmp_yr  <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$compCycles]
                            base_yr <= cmp_yr},
                          TRUE)
    
    dataset_levels <- base::unique(
      if (base_on_top) {base::c(base_label, cmp_label)
      } else {base::c(cmp_label, base_label)})
    
    base_half$Dataset <- base_label
    cmp_half$Dataset  <- cmp_label
    
    sp_levels <- unique(base_half$Species)
    
    # only group species labels when comparison is selected
    if (!base::identical(input$CompareType, "None")) {
      combined <- dplyr::bind_rows(
        base_half %>% 
          dplyr::mutate(Species = base::factor(base::as.character(Species), levels = sp_levels),
                        Dataset = base_label),
        cmp_half %>% 
          dplyr::mutate(Species = base::factor(base::as.character(Species), levels = sp_levels),
                        Dataset = cmp_label)) %>%
        dplyr::arrange(Species, base::factor(Dataset, levels = dataset_levels)) %>%
        dplyr::group_by(Species) %>%
        dplyr::mutate(Species = dplyr::if_else(
          dplyr::row_number() == 1,
          as.character(Species),
          "")) %>%
        dplyr::ungroup() %>%
        dplyr::select(Species, Dataset, Mean, Lower95, Upper95)
      
      fmt(combined)}
    
    fmt(combined)})
  
  output$densMessage <- shiny::renderUI({
    if (input$densSpeciesType == "Pick" && (base::is.null(input$densSpecies) || base::length(input$densSpecies) == 0)) {
      shiny::helpText("Please select a park and one or more species to display the table.")
    } else { NULL }})
  
  output$densTable <- DT::renderDataTable({
    shiny::req(!(input$densSpeciesType == "Pick" && (base::is.null(input$densSpecies) || base::length(input$densSpecies) == 0)))
    DT::datatable(tempDensTable(), options = base::list(dom = "t", pageLength = -1, ordering = FALSE))})
  
  #### Table Download ####
  output$densTableDownload <- shiny::downloadHandler(
    filename = function() {base::paste0(gsub("[^A-Za-z0-9_\\-]+", "_", tempDensTableTitle()), ".csv")},
    content = function(file) {
      df <- tempDensTable()
      shiny::req(!base::is.null(df), base::nrow(df) > 0)
      utils::write.csv(df, file, row.names = FALSE)})

  #### IV Plots ####
  
  #### Park Control for IVPlot ####
  output$IVParkControl <- shiny::renderUI({
    shiny::selectizeInput(inputId = "IVPark", choices = PARKLIST, label = "Park:",
                          options = base::list(
                            placeholder = 'Select a park',
                            onInitialize = base::I('function() { this.setValue(""); }')))})
  
  # IV Cycles
  output$IVCycleControl <- shiny::renderUI({
    shiny::req(DATACYCLES)
    shiny::selectInput(
      inputId = "IVCycles",
      label = "Display data from years:",
      choices = base::rev(stats::setNames(
        base::as.character(DATACYCLES$Cycle),
        base::paste0(DATACYCLES$Name, ":", DATACYCLES$YearStart, "-", DATACYCLES$YearEnd))))})
  
  #IV Species Control
  IVSpecList <- shiny::reactive({
    shiny::req(input$IVPark, input$IVGroup)
    shiny::req(input$IVPark %in% base::names(VEGDATA))
    SpecTemp <- base::unique(NPSForVeg::getPlants(
      object = VEGDATA[[input$IVPark]], group = input$IVGroup,
      years = IVYears(), common = FALSE)$Latin_Name)
    SpecNames <- fmt_common(base::tryCatch(NPSForVeg::getPlantNames(object=VEGDATA[[input$IVPark]], names=SpecTemp, in.style="Latin", 
                                                                    out.style=base::ifelse(input$IVCommon,"common","Latin")), error = function(e) SpecTemp))
    base::names(SpecTemp) <- SpecNames
    SpecTemp[order(base::names(SpecTemp))]
  })
  
  ### create reactive species count ###
  IVSpeciesCount <- shiny::reactive({
    shiny::req(input$IVPark, input$IVGroup, IVYears())
    veg <- VEGDATA[[input$IVPark]]
    shiny::req(veg)
    out <- NPSForVeg::getPlants(object = veg, group = input$IVGroup, years = IVYears(), common = FALSE)
    base::length(base::unique(out$Latin_Name))})
  
  output$IVSpeciesControl <- shiny::renderUI({
    base::switch(input$IVSpeciesType,
                 Common = {max_sp <- IVSpeciesCount()
                 htmltools::tags$div(title = "Select the maximum number of species to plot",
                                              shiny::sliderInput("IVTop", "Maximum number of species to plot (in order of IV):",
                                                                 min = 1, max = base::max(1, max_sp, na.rm = TRUE), value = base::min(5, max_sp), step = 1, ticks = FALSE))},
                 Pick = if (base::is.null(input$IVPark) || base::nchar(input$IVPark) == 0) {base::return()
                 } else {htmltools::tags$div(title = "Click here to pick the species you want to graph",
                                       shiny::selectizeInput(inputId = "IVSpecies", label = "Select one or more species",
                                                             choices = IVSpecList(), multiple = TRUE, selected = input$IVSpecies,
                                                             options = base::list(placeholder='Select species to display',
                                                                                  plugins = base::list("remove_button"))))},
                 All = NULL)})
  
  shiny::observeEvent(input$IVCommon, {
    shiny::req(input$IVSpeciesType == "Pick")
    current <- shiny::isolate(input$IVSpecies)
    shiny::updateSelectizeInput(session, "IVSpecies", choices = IVSpecList(), selected = current)})
  
  IVYears <- shiny::reactive({
    shiny::req(input$IVCycles)
    (DATACYCLES %>% dplyr::filter(Cycle == input$IVCycles) %>% dplyr::pull(YearStart)) :
      (DATACYCLES %>% dplyr::filter(Cycle == input$IVCycles) %>% dplyr::pull(YearEnd))})
  
  #### Title for IVPlot ####
  IVTitleGroup <- shiny::reactive({
    base::switch(input$IVGroup,
                 trees = "Tree",
                 saplings = "Sapling",
                 seedlings = "Tree Seedling",
                 shseedlings = "Shrub Seedling")})
  
  IVTitle <- shiny::reactive({
    base::paste0(
      NPSForVeg::getNames(VEGDATA[[input$IVPark]], "long"), ": \n",
      IVTitleGroup(), " Importance Values ",
      base::min(IVYears()), "-", base::max(IVYears()))})
  
  #### IV Data ####
  
  IVData <- shiny::reactive({
    shiny::req(input$IVPark, base::nchar(input$IVPark) > 0)
    shiny::req(IVYears())
    
    # get latin names
    raw_latin <- NPSForVeg::IV(
      object = VEGDATA[[input$IVPark]],
      group = input$IVGroup,
      years = IVYears(),
      common = FALSE)
    
    raw <- NPSForVeg::IV(
      object = VEGDATA[[input$IVPark]],
      group = input$IVGroup,
      years = IVYears(),
      common = input$IVCommon)
    
    raw$Species <- fmt_common(raw$Species)
    
    # LabelOpp = the opposite of whatever Species currently is
    if (base::isTRUE(input$IVCommon)) {
      raw$LabelOpp <- raw_latin$Species
    } else {
      raw$LabelOpp <- fmt_common(base::tryCatch(
        NPSForVeg::getPlantNames(
          object = VEGDATA[[input$IVPark]],
          names = raw$Species,
          in.style = "Latin",
          out.style = "common"),
        error = function(e) raw$Species))}
    
    raw$LabelOpp <- base::ifelse(base::is.na(raw$LabelOpp) | !base::nzchar(raw$LabelOpp), fmt_common(raw$Species), raw$LabelOpp)

    raw})
  
  ### IV checkbox ###
  iv_text_on <- shiny::reactive(base::isTRUE(input$IVPlotlyText))
  
  # IV Colors
  pickColor <- function(val, fallback) {
    if (base::is.null(val) || base::length(val) == 0 || base::is.na(val) || !base::nzchar(val)) base::return(fallback)
    val <- base::as.character(val)
    ok <- base::tryCatch({ grDevices::col2rgb(val); TRUE }, error = function(e) FALSE)
    if (!ok) fallback else val}
  
  # convert to hex to prevent default
  toHex <- function(col) {
    rgb <- grDevices::col2rgb(col)
    grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)}
  # assign contrasting text color to background color
  contrastColor <- function(bg) {
    rgb <- grDevices::col2rgb(bg)
    lum <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
    if (lum > 0.6) "black" else "white"}
  
  IVBaseColor  <- shiny::reactive(toHex(pickColor(input$IVBaseColor, "green4")))
  IVDensityColor <- shiny::reactive(toHex(pickColor(input$IVDensityColor, "green4")))
  IVSizeColor <- shiny::reactive(toHex(pickColor(input$IVSizeColor, "chartreuse")))
  IVDistributionColor <- shiny::reactive(toHex(pickColor(input$IVDistributionColor, "yellow")))
  
  #### IV Plot ####
  tempIVPlot <- shiny::reactive({
    shiny::req(IVData())
    
    if (base::is.null(input$IVPark) || base::nchar(input$IVPark) == 0) {
      shiny::validate(shiny::need(input$IVPark, "Select a park to display the graph."))}
    
    IVdf <- IVData()
    
    # sets number of displayed species
    IVdf <- base::switch(input$IVSpeciesType, 
                         Common = {shiny::req(input$IVTop)
                           IVdf %>%
                             dplyr::slice_max(order_by = Total, n = input$IVTop, with_ties = FALSE) %>%
                             dplyr::arrange(Total)},
                         Pick = {shiny::req(input$IVSpecies)
                           IVdf %>%
                             dplyr::filter(
                               if (base::isTRUE(input$IVCommon)) LabelOpp %in% input$IVSpecies
                               else Species %in% input$IVSpecies) %>%
                             dplyr::arrange(Total)},
                         All = IVdf %>%
                           dplyr::summarise(
                             Species = "All species ",
                             Density = base::round(base::mean(Density, na.rm = TRUE), 2),
                             Size = base::round(base::mean(Size, na.rm = TRUE), 2),
                             Distribution = base::round(base::mean(Distribution, na.rm = TRUE), 2),
                             Total = base::round(base::mean(Total, na.rm = TRUE), 2),
                             LabelOpp = "All species") %>%
                           dplyr::arrange(Total))
    
    ### plot in plotly ###
    if (!input$IVPart) {
      p <- plotly::plot_ly(
        data = IVdf,
        x = ~Total,
        y = ~Species,
        type = "bar",
        orientation = "h",
        name = "Total IV",
        marker = base::list(color = IVBaseColor()),
        text = if (iv_text_on()) {~base::sprintf("Species: %s<br>Total IV: %.2f", LabelOpp, Total)} else {NULL},
        hovertext = ~base::sprintf("Species: %s<br>Total IV: %.2f", LabelOpp, Total),
        hoverinfo = if (iv_text_on()) {"none"} else {"text"},
          hoverlabel = base::list(
            bgcolor = IVBaseColor(),
          bordercolor = "white",
          font = base::list(size = 14, color = contrastColor(IVBaseColor()))))
    } else {
      p <- plotly::plot_ly() %>%
        plotly::add_trace(
          data = IVdf, x = ~Density, y = ~Species,
          name = "Density", type = "bar", orientation = "h",
          legendrank = 3,
          marker = base::list(color = IVDensityColor()),
          text = if (iv_text_on()) {~base::sprintf("%.2f", Density)} else {NULL},
          hovertext = ~base::sprintf("Species: %s<br>Density: %.2f", LabelOpp, Density),
          hoverinfo = "text",
          hoverlabel = base::list(bgcolor = IVDensityColor(), bordercolor = "white",
          font = base::list(size = 14, color = contrastColor(IVDensityColor())))) %>%
        plotly::add_trace(
          data = IVdf, x = ~Size, y = ~Species,
          name = "Size", type = "bar", orientation = "h",
          legendrank = 2,
          marker = base::list(color = IVSizeColor()),
          text = if (iv_text_on()) {~base::sprintf("%.2f", Size)} else {NULL},
          hovertext = ~base::sprintf("Species: %s<br>Size: %.2f", LabelOpp, Size),
          hoverinfo = "text",
          hoverlabel = base::list(bgcolor = IVSizeColor(), bordercolor = "white",
          font = base::list(size = 14, color = contrastColor(IVSizeColor())))) %>%
        plotly::add_trace(
          data = IVdf, x = ~Distribution, y = ~Species,
          name = "Distribution", type = "bar", orientation = "h",
          legendrank = 1,
          marker = base::list(color = IVDistributionColor()),
          text = if (iv_text_on()) {~base::sprintf("%.2f", Distribution)} else {NULL},
          hovertext = ~base::sprintf("Species: %s<br>Distribution: %.2f", LabelOpp, Distribution),
          hoverinfo = "text",
          hoverlabel = base::list(bgcolor = IVDistributionColor(), bordercolor = "white",
                                  font = base::list(size = 14, color = contrastColor(IVDistributionColor()))))}
    
    IVFontSize <- if (!base::is.null(input$IVFontSize)) input$IVFontSize else 12
    densFontSize  <- shiny::reactive(if (!base::is.null(input$densFontSize)) input$densFontSize else 12)
    
    p <- p %>% plotly::layout(
      barmode = "stack",
      showlegend = TRUE,
      legend = base::list(
        traceorder  = "reversed",
        font = base::list(size = IVFontSize + 3),
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = 1,   yanchor = "bottom"),
      title = base::list(
        text = IVTitle(),
        font = base::list(size = IVFontSize + 10),
        y = 1,
        yanchor = "top",
        pad = base::list(t = 20)),
      xaxis = base::list(
        title = "",
        tickvals = base::c(0, base::max(IVdf$Total)),
        ticktext = base::c("Low", "High"),
        tickfont = base::list(size = IVFontSize)),
      yaxis = base::list(
        title = "",
        tickfont = base::list(size = IVFontSize),
        ticks = "outside",
        ticklabelposition = "outside",
        categoryorder = "array",
        categoryarray = IVdf$Species),
      margin = base::list(t = 80, l = 140, r = 40),
      font = base::list(size = 12),
      autosize = TRUE)
    p})
  
  output$IVPlot <- plotly::renderPlotly({
    shiny::validate(shiny::need(
      !base::is.null(input$IVPark) && base::nzchar(input$IVPark),
      "Select a park to display the graph."))
    tempIVPlot()})
  
  #### jpeg Plot Download ####
  #output$IVGraphDownload<-shiny::downloadHandler(
  #  filename=function(){base::paste(IVTitle(), ".jpeg", sep="")}, 
  #  content=function (file){
  #    grDevices::jpeg(file,width=15,height=6,units="in",res=300, quality=100)
  #    base::print(tempIVPlot())
  #    grDevices::dev.off()
  #  }
  #)
  
  #### wmf plot download ####
  #output$IVWmfDownload<-shiny::downloadHandler(
  #  filename=function(){base::paste(IVTitle(), ".wmf", sep="")}, 
  #  content=function (file){
  #    grDevices::win.metafile(file,width=15,height=6)
  #   base::print(tempIVPlot())
  #    grDevices::dev.off()
  #  }
  #)
  
  
#  IVTableArgs<-shiny::reactive({
#    base::list(
#      object=IVPlotArgs()$object,
#      group=IVPlotArgs()$IVargs$group, 
#      years=IVPlotArgs()$IVargs$years, 
#      common=IVPlotArgs()$IVargs$common
#    )
#})
 
  #### IV Table ####
  #### title ####
  
  tempIVTableTitle<-shiny::reactive({ 
    shiny::validate(shiny::need(base::try(IVTitle()), message=FALSE) )
    IVTitle()
  })
  
  ## Table
  output$IVTableTitle<-shiny::renderText({tempIVTableTitle() })
  
  #tempIVTable <- shiny::reactive({
  #  shiny::validate(shiny::need(
  #    !base::is.null(input$IVPark) && base::nzchar(input$IVPark),
  #    "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."))
  #  df <- IVData()
  #  shiny::validate(shiny::need(!base::is.null(df) && base::nrow(df) > 0,
  #    "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."))
  #  df %>% dplyr::select(-LabelOpp)})
  
  
  
  tempIVTable <- shiny::reactive({
    shiny::validate(shiny::need(
      !base::is.null(input$IVPark) && base::nzchar(input$IVPark),
      "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."))
    
    df <- IVData()
    
    shiny::validate(shiny::need(
      !base::is.null(df) && base::nrow(df) > 0,
      "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."))
    df <- base::switch(input$IVSpeciesType,
                       Common = {
                         shiny::req(input$IVTop)
                         df %>%
                           dplyr::slice_max(order_by = Total, n = input$IVTop, with_ties = FALSE) %>%
                           dplyr::arrange(dplyr::desc(Total))},
                       Pick = {
                         shiny::req(input$IVSpecies)
                         df %>%
                           dplyr::filter(
                             if (base::isTRUE(input$IVCommon)) LabelOpp %in% input$IVSpecies
                             else Species %in% input$IVSpecies) %>%
                           dplyr::arrange(dplyr::desc(Total))},
                       All = df %>%
                         dplyr::summarise(
                           Species = "All species",
                           Density = base::round(base::mean(Density, na.rm = TRUE), 2),
                           Size = base::round(base::mean(Size, na.rm = TRUE), 2),
                           Distribution = base::round(base::mean(Distribution, na.rm = TRUE), 2),
                           Total = base::round(base::mean(Total, na.rm = TRUE), 2)) %>%
                         dplyr::arrange(Total))
    
    if (!input$IVPart) {df <- df %>% dplyr::select(Species, Total)
    } else {df <- df %>% dplyr::select(Species, Density, Size, Distribution, Total)}
    df})

  output$IVMessage <- renderUI({
    if (input$IVSpeciesType == "Pick" && (base::is.null(input$IVSpecies) || base::length(input$IVSpecies) == 0)) {
      helpText("Please select a park and one or more species to display the table.")
    } else {NULL}})
  output$IVData <- DT::renderDataTable({
    shiny::req(!(input$IVSpeciesType == "Pick" && (base::is.null(input$IVSpecies) || base::length(input$IVSpecies) == 0)))
    
    DT::datatable(tempIVTable(), options = base::list(dom = "t", pageLength = -1, ordering = FALSE, 
                                                      columnDefs = base::list(base::list(className = 'dt-left', targets = "_all")))) %>%
      DT::formatRound(columns = base::intersect(base::c("Density", "Size", "Distribution", "Total"), base::names(tempIVTable())), digits = 3)})

  #### IV Table download ####
  output$IVTableDownload <- shiny::downloadHandler(
  filename = function() {base::paste0(base::gsub("[^A-Za-z0-9_\\-]+", "_", tempIVTableTitle()), ".csv")},
  content = function(file) {df <- tempIVTable()
    shiny::req(!base::is.null(df), base::nrow(df) > 0)
    utils::write.csv(df, file, row.names = FALSE)})
  
  #### Species list ####
  #### Species list park control ####
  output$SpListParkControl<-shiny::renderUI({
    shiny::validate(
      shiny::need(PARKLIST, message=FALSE )
    )
    shiny::selectizeInput(inputId="SpListPark", choices=PARKLIST, label="Park:",
                          options = base::list(placeholder='Select a park', onInitialize = base::I('function() { this.setValue(""); }'))
    ) 
  })
  
  
  #### Species list plot control ####
  
  output$SpListPlotControl <-shiny::renderUI({
    shiny::validate(
      shiny::need(input$SpListPark!="", message = F)
    )
    shiny::selectizeInput(inputId="SpListPlot", choices=base::c("All Plots"="All", NPSForVeg::getPlotNames(VEGDATA[[input$SpListPark]],type="all")),
                          label="Plots (optional)", multiple=TRUE, selected="All", options = base::list(plugins = base::list("remove_button"))
    )
  })
  
  
  SpListPlotUse<-shiny::reactive({
    if(base::length(input$SpListPlot)==0 || "All" %in%  input$SpListPlot ) base::return(NA) else base::return(input$SpListPlot)
    
  })
  
  LatinList<-shiny::reactive({
    shiny::validate(
      shiny::need(input$SpListPark, message=FALSE)  
    )
    base::unique(base::c(
      NPSForVeg::getPlants(object=VEGDATA[[input$SpListPark]], group="trees", plots=SpListPlotUse())$Latin_Name,
      NPSForVeg::getPlants(object=VEGDATA[[input$SpListPark]], group="saplings",plots=SpListPlotUse())$Latin_Name,
      NPSForVeg::getPlants(object=VEGDATA[[input$SpListPark]], group="seedlings", plots=SpListPlotUse())$Latin_Name,
      NPSForVeg::getPlants(object=VEGDATA[[input$SpListPark]], group="shrubs", plots=SpListPlotUse())$Latin_Name,
      NPSForVeg::getPlants(object=VEGDATA[[input$SpListPark]], group="shseedlings", plots=SpListPlotUse())$Latin_Name,
      NPSForVeg::getPlants(object=VEGDATA[[input$SpListPark]], group="vines", plots=SpListPlotUse())$Latin_Name,
      NPSForVeg::getPlants(object=VEGDATA[[input$SpListPark]], group="herbs", plots=SpListPlotUse())$Latin_Name
    ))
  })
  
  decapitalize <- function(string) {     ########### used to hack around sorting/encoding issues
    base::substr(string, 1, 1) <- base::toupper(base::substr(string, 1, 1))
    base::return(string)
  }
  
  CommonList <- shiny::reactive({
    fmt_common(NPSForVeg::getPlantNames(object = VEGDATA[[input$SpListPark]],
                                          names = LatinList(), out.style = "common", in.style = "Latin"))})  
  
  MonitoringList <- shiny::reactive({
    tibble::tibble(
      `Latin Name` = LatinList(),
      `Common Name` = CommonList()) |>
      dplyr::arrange(`Common Name`) |>
      dplyr::select(`Common Name`, `Latin Name`)})
  
  ###Make URL for and get data from NPSpecies
  NPSpeciesURL<-shiny::reactive({base::paste0("https://irmaservices.nps.gov/v3/rest/npspecies/checklist/",input$SpListPark,"/Vascular%20Plant?format=Json")})
  
#  NPSpeciesList<-shiny::reactive({
#    jsonlite::fromJSON(NPSpeciesURL()) %>% 
#      dplyr::select(CommonNames,ScientificName,Occurrence) %>% 
#      dplyr::arrange(CommonNames) %>% 
#      dplyr::rename("Latin Name"=ScientificName, "Common Name"=CommonNames)
#  })  
  
  ### bugfix ##############################################
  NPSpeciesList <- shiny::reactive({
    parsed <- jsonlite::fromJSON(NPSpeciesURL())
    
    shiny::validate(
      shiny::need(base::length(parsed) > 0,
                  "There is no Natioanl Park Species data available for this park."))
    
    parsed |>
      dplyr::select(CommonNames, ScientificName, Occurrence) |>
      dplyr::arrange(CommonNames) |>
      dplyr::rename(`Latin Name` = ScientificName, `Common Name` = CommonNames)})
  #########################################################
  
  ##Create Title for Table
  SpeciesTableTitle <- shiny::reactive({
    shiny::req(input$SpListPark)
    
    base_obj <- VEGDATA[[input$SpListPark]]
    shiny::req(base_obj)
    
    park_name <- NPSForVeg::getNames(base_obj, "long")
    
    base::switch(input$SpListType,
                 Monitoring = base::paste0("Vascular Plant Species Found in ", park_name, " Monitoring Plots"),
                 NPSpecies = base::paste0("All Vascular Plant Species Known from ", park_name))})
  
  SpeciesTableData <- shiny::reactive({
    base::switch(input$SpListType,
                 Monitoring = MonitoringList(),
                 NPSpecies  = NPSpeciesList())})
  
  output$SpeciesTableTitle <- shiny::renderText({
    SpeciesTableTitle()})
  
  output$SpeciesTable <- DT::renderDataTable({
    shiny::validate(shiny::need(input$SpListPark != "",
                                message = "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."))
    df <- SpeciesTableData()
    
    shiny::validate(shiny::need(base::is.data.frame(df), "Data not available."))
    
    DT::datatable(df, rownames = FALSE, selection = "single", class = "display compact") |>
      DT::formatStyle("Latin Name", fontStyle = "italic")})
  
  output$NPSpeciesLink <- shiny::renderUI({
    shiny::req(input$SpListPark)
    if (input$SpListType != "NPSpecies") {base::return(NULL)}
    
    link_info <- switch(input$SpListPark, 
      "ANTI" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/ANTI",
        label = "View full Antietam species list"),
      "CATO" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/CATO",
        label = "View full Catoctin species list"),
      "CHOH" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/CHOH",
        label = "View full C&O Canal species list"),
      "GWMP" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/GWMP",
        label = "View full GW Parkway species list"),
      "HAFE" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/HAFE",
        label = "View full Harpers Ferry species list"),
      "MANA" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/MANA",
        label = "View full Manassas species list"),
      "MONO" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/MONO",
        label = "View full Monocacy species list"),
      "NACE" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/NACE",
        label = "View full National Capital Parks – East species list"),
      "PRWI" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/PRWI",
        label = "View full Prince William species list"),
      "ROCR" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/ROCR",
        label = "View full Rock Creek species list"),
      "WOTR" = list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/WOTR",
        label = "View full Wolf Trap species list"),
      
      list(url = base::paste0("https://irma.nps.gov/NPSpecies/Search/SpeciesList/",
          input$SpListPark), label = "View full NPSpecies list"))
    
    shiny::tags$a(href = link_info$url, target = "_blank", link_info$label)})
  
  #### Table Download ####
  output$SpeciesTableDownload <- shiny::downloadHandler(
    filename = function() {
      base::paste0(base::gsub("[^A-Za-z0-9_\\-]+", "_", SpeciesTableTitle()), ".csv")},
    
    content = function(file) {shiny::req(input$SpListPark != "")
      df <- SpeciesTableData()
      shiny::req(base::is.data.frame(df), base::nrow(df) > 0)
      utils::write.csv(df, file, row.names = FALSE)})

})# end of shiny::shinyServer() function

