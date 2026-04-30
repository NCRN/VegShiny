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
                       choices=base::rev(stats::setNames(base::as.character(DATACYCLES$Cycle), base::paste0(DATACYCLES$Name,": ",
                                                                                                            DATACYCLES$YearStart,"-",DATACYCLES$YearEnd)))
    )
  })
  
  MapYears<-shiny::reactive({
    shiny::req(input$MapCycles)
    (DATACYCLES %>% dplyr::filter(Cycle==input$MapCycles) %>% dplyr::pull(YearStart)) : 
      (DATACYCLES %>% dplyr::filter(Cycle==input$MapCycles) %>% dplyr::pull(YearEnd))
  })
  
  # Map MetaData
  MapMetaData<-shiny::reactive({
    shiny::req(input$MapValues, input$MapGroup)
    MAPLEGEND[[input$MapValues]][[input$MapGroup]] 
  })
  
  # Data to plot on map - always for all parks 
  MapData<-shiny::reactive({
    shiny::req(input$MapSpecies=="All" | input$MapSpecies %in% NPSForVeg::getPlants(object=VEGDATA, group=input$MapGroup, years=MapYears())$Latin_Name )
    shiny::req(input$MapGroup!="vines" | (input$MapGroup=="vines" & input$MapValues=="count"))
    
    P<-dplyr::left_join(NPSForVeg::getPlots(VEGDATA, years=MapYears(), output="dataframe", type="all") %>% 
                          dplyr::select(Plot_Name,Unit_Code, Latitude, Longitude), NPSForVeg::getEvents(object=VEGDATA, years=MapYears(), plot.type="all") %>% 
                          dplyr::select(Plot_Name,Year=Event_Year), by="Plot_Name") %>% 
      dplyr::mutate(Size=NPSForVeg::getArea(VEGDATA[Unit_Code], group=input$MapGroup))
    
    # if(input$MapGroup != "herbs"){
    #   base::return(P %>% 
    #            dplyr::left_join(NPSForVeg::SiteXSpec(object=VEGDATA, group=input$MapGroup, years=MapYears(), 
    #                                status=if(input$MapGroup=='trees') {
    #                                  shiny::req(input$TreeStatus)
    #                                  input$TreeStatus
    #                                  } else {'alive'},
    #                    species= if(input$MapSpecies=="All") NA else input$MapSpecies, values=input$MapValues, area="ha") %>% 
    #            dplyr::select(Plot_Name,Values=Total), by="Plot_Name")
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
      
      spec_data <- NPSForVeg::SiteXSpec(
        object = VEGDATA,
        group = input$MapGroup,
        years = MapYears(),
        status = status_val,
        species = species_val,
        values = input$MapValues,
        area = "ha"
      )
      
      base::return(P %>% dplyr::left_join(spec_data %>% dplyr::select(Plot_Name, Values = Total), by = "Plot_Name"))
    }
    
    if(input$MapGroup == "herbs"){
      base::return(P %>% 
                     dplyr::mutate(Values=NPSForVeg::SiteXSpec(object=VEGDATA,group=input$MapGroup, years=MapYears(),
                                                               species= if(input$MapSpecies=="All") NA else input$MapSpecies,
                                                               values=input$MapValues)$Total)#/getArea(VEGDATA[Unit_Code], group=input$MapGroup, type="count"))
      )
    }
  })
  
  # Map Colors
  CircleColors<-shiny::reactive({
    shiny::req(MapMetaData()$Cuts)
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
  
  
  # add Monitoring plot data as circles - needs to be before layers or app hangs for some reason - new issue?
  shiny::observe({
    shiny::req(MapData()$Values)
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
    leaflet::leafletProxy("VegMap") %>%
      leaflet::removeControl(layerId="CircleLegend") %>%
      leaflet::addLegend(.,title=MapMetaData()$Title,
                         colors=CircleColors()(MapMetaData()$Cuts[-1]-.001),
                         labels=MapMetaData()$Labels,
                         layerId="CircleLegend",
                         opacity=1)
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
      cn <- cn %>%
        dplyr::filter(language %in% base::c("English", "unspecified")) %>%
        dplyr::mutate(name_word_count = base::lengths(base::strsplit(commonName, "\\s+"))) %>%
        dplyr::filter(!(name_word_count == 1 & base::any(name_word_count >= 2))) %>%
        dplyr::slice(1)
      
      cn_list[[i]] <- cn
    }
    
    cn_df <- base::do.call(base::rbind, base::lapply(cn_list, base::as.data.frame))
    
    if (base::nrow(cn_df) == 0) base::return(NULL)
    
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
    vd_filled <- vegdata_df %>%
      dplyr::left_join(cn_df, by = "TSN", suffix = base::c("", "_new")) %>%
      dplyr::mutate(
        Common = base::ifelse(Common == "" | base::is.na(Common), Common_new, Common)
      ) %>%
      dplyr::select(-Common_new) %>%
      dplyr::mutate(Common = base::ifelse(TSN == "25328", "spirea", Common)) %>%
      dplyr::distinct(Latin_Name, Common, .keep_all = FALSE)
    
    new_vd_rows <-base::data.frame(
      Latin_Name = base::c("Acer spp.", "Quercus acutissima", "Oplismenus undulatifolius", "Robinia viscosa", "Viburnum lantana", "Rosaceae Family", "Lygodium palmatum"),
      Common = base::c("maples", "sawtooth oak", "wavyleaf basketgrass", "clammy locust", "wayfaring tree", "roses", "American climbing fern")
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
    
    SpecNames <- safeGetPlantNames(object = vd_filled,
                                   names = SpecTemp,
                                   in.style = "Latin",
                                   out.style = base::ifelse(input$mapCommon, "common", "Latin"))
    
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
    print(base::paste0('input$MapPark:', input$MapPark))
    print(base::paste0('input$MapGroup:', input$MapGroup))
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
    
    ShapeOver<-input$VegMap_shape_mouseover
    selectedPlot <- MapData()[MapData()$Plot_Name == ShapeOver$id,]
    
    
    leaflet::leafletProxy("VegMap") %>%
      leaflet::clearPopups() %>% {
        base::switch(ShapeOver$group,
                     Circles= leaflet::addPopups(map=.,lat=ShapeOver$lat+.001, lng=ShapeOver$lng, layerId="MouseOverPopup",
                                                 popup=base::paste0(
                                                   shiny::h5(NPSForVeg::getNames(VEGDATA[[selectedPlot$Unit_Code]], "long")),
                                                   shiny::h6("Monitoring Plot:",selectedPlot$Plot_Name),
                                                   shiny::h6("Year Monitored:", selectedPlot$Year),
                                                   shiny::h6(base::names(MapSpecList()[MapSpecList()==input$MapSpecies]),":",base::format(base::signif(selectedPlot$Values,2),
                                                                                                                                          big.mark=","), " ", MapMetaData()$Title),
                                                   htmltools::tags$h6("Click on plot to see full list")
                                                 )
                     )
        )}
  })
  
  shiny::observeEvent(input$VegMap_shape_mouseout,{    #clear popup when mouse leaves circle
    leaflet::leafletProxy("VegMap") %>%
      leaflet::clearPopups()
  })
  
  # Mouse Click 
  
  shiny::observeEvent(input$VegMap_shape_click, {          # user clicked on a shape
    ShapeClick<-input$VegMap_shape_click
    selectedPlot <- MapData()[MapData()$Plot_Name == ShapeClick$id,]
    
    if(
      base::class(base::try(NPSForVeg::SiteXSpec(object=VEGDATA[[selectedPlot$Unit_Code]], group=input$MapGroup, years=selectedPlot$Year,
                                                 plots=ShapeClick$id, common=input$mapCommon,
                                                 status= if(input$MapGroup=='trees') input$TreeStatus else 'alive' 
      ), silent=TRUE))=="try-error") {
      content<-base::as.character(htmltools::tagList(htmltools::tags$h6("None found on this plot")))
    } else {
      
      tempData<- if(input$MapGroup != "herbs"){ 
        NPSForVeg::SiteXSpec(object=VEGDATA[[selectedPlot$Unit_Code]],group=input$MapGroup, years=selectedPlot$Year,
                             plots=ShapeClick$id, values=input$MapValues,area="ha", common=input$mapCommon,
                             status= if(input$MapGroup=='trees') input$TreeStatus else 'alive'
        )[-1]
        
      } else {
        
        
        if(input$MapGroup == "herbs"){
          NPSForVeg::SiteXSpec(object=VEGDATA[[selectedPlot$Unit_Code]], group=input$MapGroup, years=selectedPlot$Year,
                               plots=ShapeClick$id,values=input$MapValues,common=input$mapCommon)[-1]
        }
      }
      
      
      content<-base::paste0( shiny::h5(NPSForVeg::getNames(VEGDATA[[selectedPlot$Unit_Code]],"long")),
                             shiny::h6("Monitoring Plot:",selectedPlot$Plot_Name),
                             shiny::h6("Year Monitored:",selectedPlot$Year),
                             shiny::h6("Species: ",MapMetaData()$Title),
                             htmltools::tagList(htmltools::tags$table(
                               base::mapply(FUN=function(Name,Value){
                                 htmltools::tags$tr(
                                   htmltools::tags$td(base::sprintf("%s:  ", Name)),
                                   htmltools::tags$td(align="right",base::sprintf("%s", base::format(base::signif(Value,2), big.mark=",")))
                                 )
                               },
                               Name=base::names(tempData),
                               Value=base::unlist(tempData), SIMPLIFY=FALSE
                               )))
                             
      )
    }
    
    leaflet::leafletProxy("VegMap") %>%
      leaflet::clearPopups() %>% {
        base::switch(ShapeClick$group,
                     Circles= leaflet::addPopups(map=.,lat=ShapeClick$lat+.001, lng=ShapeClick$lng, layerId="CircleClickPopup",popup=content),
                     Ecoregion=, Forested=, Soil= leaflet::addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$id)
        )}
    
  })
  
  
  
  
  
  
  
  
  
  
  
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
                       choices=base::rev(stats::setNames(base::as.character(DATACYCLES$Cycle), base::paste0(DATACYCLES$Name,": ",
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
    SpecTemp<-base::unique(NPSForVeg::getPlants(object=VEGDATA[[input$densPark]], group=input$densGroup,  years=densYears(),common=F )$Latin_Name)
    SpecNames<-NPSForVeg::getPlantNames(object=VEGDATA[[input$densPark]], names=SpecTemp, in.style="Latin",
                                        out.style=base::ifelse(input$densCommon,"common","Latin"))
    base::names(SpecTemp)<-SpecNames  
    SpecTemp<-SpecTemp[order(base::names(SpecTemp))]
  })
  
  output$densSpeciesControl<-shiny::renderUI({
    base::switch(input$densSpeciesType,
                 Common= htmltools::tags$div(title="Select the maximum number of species to plot", 
                                             shiny::sliderInput(inputId="densTop",label="Maximum number of species to plot (in order of mean value):",
                                                                min=1, max=10,value=5, sep="", step=1, ticks=TRUE)
                 ),
                 Pick= if(base::is.null(input$densPark) || base::nchar(input$densPark)==0) {  base::return()  }
                 else{
                   htmltools::tags$div(title="Click here to pick the species you want to graph",
                                       shiny::selectizeInput(inputId="densSpecies", label="Select one or more species,
              backspace to remove", choices=densSpecList(),
                                                             multiple=TRUE, selected = input$densSpecies)
                   )
                 }
    )
  })
  
  ### keep species selction when toggling between common/latin names ###
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
                                                             choices=base::rev(stats::setNames(base::as.character(DATACYCLES$Cycle), base::paste0(DATACYCLES$Name,": ",
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
  #                     values=input$densvalues, Total=F, common=F) %>% dplyr::arrange(desc(Mean)) %>% slice(1:input$densTop) %>% dplyr::pull(Latin_Name))
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
  
  DensCompare<-shiny::reactive({shiny::req(input$CompareType, input$densPark, input$densvalues)
    shiny::req(growthStageGuard())
    base::switch(input$CompareType,
                 None=base::return(NA),
                 Park = {
                   if (base::is.null(input$ComparePark) || !nzchar(input$ComparePark)) return(NA)
                   if (base::identical(input$ComparePark, "ALL")) {
                     return(list(
                       object = NULL,
                       park   = "ALL",
                       group  = input$densGroup,
                       years  = densYears(),
                       values = input$densvalues,
                       common = input$densCommon,
                       area   = if (input$densvalues == "size") "ha" else "plot"))}
                   return(list(
                     object = VEGDATA[[input$ComparePark]],
                     park   = input$ComparePark,
                     group  = input$densGroup,
                     years  = densYears(),
                     values = input$densvalues,
                     common = input$densCommon,
                     area   = if (input$densvalues == "size") "ha" else "plot"))},
                 "Growth Stage"=base::return(base::list(object=VEGDATA[[input$densPark]], group=input$CompareGroup, years=densYears(),
                                                        values=input$densvalues,
                                                        common=input$densCommon,area=if(input$densvalues=="size") "ha" else "plot" )),
                 Time=base::return(base::list(object=VEGDATA[[input$densPark]], group=input$densGroup, years=compYears(),
                                              values=input$densvalues, 
                                              common=input$densCommon,area=if(input$densvalues=="size") "ha" else "plot" )))
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
  densData <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    veg <- VEGDATA[[input$densPark]]; shiny::req(veg)
    NPSForVeg::dens(
      object = veg,
      group  = input$densGroup,
      years  = densYears(),
      values = input$densvalues,
      common = FALSE,
      area   = if (input$densvalues == "size") "ha" else "plot",
      Total  = FALSE)
  })
  
  #### common names checkbox ####
  species_col <- shiny::reactive(if (base::isTRUE(input$densCommon)) "Common_Name" else "Latin_Name")
  
  ### summary statistics checkbox ###
  text_on <- shiny::reactive(base::isTRUE(input$plotlyText))
  
  #### create base plotting df #####
  densDf <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    raw <- densData()
    
    ### add common names if requested ###
    # Always add Common_Name — not only when densCommon is TRUE
    if (!("Common_Name" %in% base::names(raw))) {
      raw$Common_Name <- NPSForVeg::getPlantNames(
        object    = VEGDATA[[input$densPark]],
        names     = raw$Latin_Name,
        in.style  = "Latin",
        out.style = "common")}
    
    df <- raw %>%
      dplyr::transmute(
        Species = .data[[species_col()]],
        Latin   = .data$Latin_Name,
        Common  = if ("Common_Name" %in% base::names(raw)) .data$Common_Name else NA_character_,
        Mean    = .data$Mean,
        err_up  = .data$Upper.95 - .data$Mean,
        err_dn  = .data$Mean - .data$Lower.95) %>%
      dplyr::filter(!base::tolower(Species) %in% base::c("total", "all species"))
    
    ### radioButton: Pick ###
    if (base::identical(input$densSpeciesType, "Pick")) {
      shiny::req(input$densSpecies)
      chosen <- if (species_col() == "Latin_Name") {
        input$densSpecies
      } else {
        NPSForVeg::getPlantNames(
          object    = VEGDATA[[input$densPark]],
          names     = input$densSpecies,
          in.style  = "Latin",
          out.style = "common")}
      df <- df %>% dplyr::filter(Species %in% chosen)}
    
    ### radioButton: Common ###
    if (base::identical(input$densSpeciesType, "Common")) {
      shiny::req(input$densTop)
      df <- df %>% dplyr::arrange(dplyr::desc(Mean)) %>% dplyr::slice(1:input$densTop)}
    
    ### radioButton: All ###
    if (base::identical(input$densSpeciesType, "All")) {
      agg_fun <- if (input$densvalues %in% base::c("count","size")) sum else mean
      df <- df %>%
        dplyr::summarise(
          Species = if (base::isTRUE(input$densCommon)) "All species" else "All species",
          Mean    = agg_fun(Mean,   na.rm = TRUE),
          err_up  = agg_fun(err_up, na.rm = TRUE),
          err_dn  = agg_fun(err_dn, na.rm = TRUE))
      df <- df %>% dplyr::mutate(LabelOpp = "All species")} else {
        df <- df %>% dplyr::mutate(
          LabelOpp = dplyr::case_when(
            isTRUE(input$densCommon) ~ Latin,
            !isTRUE(input$densCommon) ~ dplyr::coalesce(Common, Latin)))}
    
    ### plot order ###
    df %>%
      dplyr::arrange(Mean) %>%
      dplyr::mutate(Species = base::factor(Species, levels = Species))
  })
  
  #### create compare plotting df #####
  
  ###bugfix update###########################
  dens_all_parks <- function(VEGDATA, group, years, values, common = FALSE) {
    per_park <- base::lapply(base::names(VEGDATA), function(pk) {
      x <- VEGDATA[[pk]]
      out <- base::tryCatch(
        NPSForVeg::dens(
          object = x,
          group  = group,
          years  = years,
          values = values,
          common = common,
          area   = if (values == "size") "ha" else "plot",
          Total  = FALSE
        ),
        error = function(e) NULL
      )
      if (base::is.null(out) || base::nrow(out) == 0) return(NULL)
      
      # Plot count for this park, used as the pooling weight
      n_plots <- base::nrow(NPSForVeg::getPlots(x, years = years, type = "all"))
      out$.n_plots <- n_plots
      out
    })
    
    per_park <- dplyr::filter(base::negate(base::is.null), per_park)
    if (!base::length(per_park)) return(NULL)
    
    combined <- dplyr::bind_rows(per_park)
    
    # Plot-count-weighted pooling across parks.
    # Mean is a proper plot-weighted network estimate.
    # CIs are approximated by weighted-averaging per-park CIs — see note in compareDf.
    pooled <- combined %>%
      dplyr::group_by(Latin_Name) %>%
      dplyr::summarise(
        Mean     = stats::weighted.mean(Mean,     .n_plots, na.rm = TRUE),
        Lower.95 = stats::weighted.mean(Lower.95, .n_plots, na.rm = TRUE),
        Upper.95 = stats::weighted.mean(Upper.95, .n_plots, na.rm = TRUE),
        .groups  = "drop"
      )
    
    pooled
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
      if (base::is.null(raw) || base::nrow(raw) == 0) return(NULL)
      
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
      if (base::is.null(raw) || base::nrow(raw) == 0) return(NULL)}
    
    ### add common names if requested ###
    
    ###bugfix update: Replace the name_obj line and getPlantNames call in compareDf#########################
    name_obj <- VEGDATA[[input$densPark]] %||% VEGDATA[[PARKLIST[1]]]
    
    if (!("Common_Name" %in% base::names(raw))) {
      raw$Common_Name <- base::tryCatch({
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
      })
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
      dplyr::filter(!tolower(Species) %in% base::c("total", "all species"))
    
    ###selection rules
    # Pick
    if (base::identical(input$densSpeciesType, "Pick")) {
      shiny::req(input$densSpecies)
      chosen <- if (species_col_val == "Latin_Name") {
        input$densSpecies
      } else {
        NPSForVeg::getPlantNames(
          object    = name_obj,   # base veg object for name mapping
          names     = input$densSpecies,
          in.style  = "Latin",
          out.style = "common")}
      df <- df %>% dplyr::filter(Species %in% chosen)}
    
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
              isTRUE(input$densCommon) ~ Latin,
              TRUE                     ~ dplyr::coalesce(Common, Latin)))}
    
    # match ordering of base df
    if (!base::identical(input$densSpeciesType, "All")) {
      base_levels <- densDf()$Species
      df$Species <- factor(df$Species, levels = levels(densDf()$Species))    }
    
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
  #                     values=input$densvalues, Total=F, common=F) %>% dplyr::arrange(desc(Mean)) %>% slice(1:input$densTop) %>% dplyr::pull(Latin_Name))
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
                 "Growth Stage" = base::return(base::paste(base_name, ":", grp_title, "vs.", compareTitleGroup(), val_title, period1)),
                 Time = base::return(base::paste(base_name, ":", grp_title, val_title, period1, "vs.", 
                                                 base::paste0(base::as.character(base::min(compYears())), "-", base::as.character(base::max(compYears()))))))})
  
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
      if (base::is.null(park_key) || !nzchar(park_key)) return(park_key)
      obj <- VEGDATA[[park_key]]
      if (!base::is.null(obj)) {return(NPSForVeg::getNames(obj, "long"))}}
    
    cycle_long_name <- function(cycle_code) {
      if (base::is.null(cycle_code) || !nzchar(cycle_code)) return(cycle_code)
      row <- DATACYCLES[DATACYCLES$Cycle == cycle_code, ]
      if (base::nrow(row) == 1L) {
        base::paste0(row$Name, ": ", row$YearStart, "-", row$YearEnd)} else {
          base::as.character(cycle_code)}}
    
    species_group_long <- stats::setNames(DENSLABELDATA$Label, DENSLABELDATA$Name)
    group_long_name <- function(group_key) {
      val <- species_group_long[group_key]
      if (base::length(val) == 0 || is.na(val)) group_key else val}
    
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
    
    ### for bar chart plotly ###  
    n_species <- base::length(species_levels)
    bars_per_species <- if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) 2 else 1
    
    
    row_px <- if (n_species == 1) 200 else 100 * bars_per_species
    
    top_pad_px    <- 50
    bottom_pad_px <- 50
    fig_height <- top_pad_px + n_species * row_px + bottom_pad_px
    
    
    #plot in plotly
    p <- plotly::plot_ly(
      data = df,
      y = ~Species,
      x = ~Mean,
      type = "bar",
      orientation = "h",
      #mode = "markers", ##for scatter plot
      name = base_legend,   
      showlegend = TRUE,        
      marker = base::list(size = 10, color = "#2385ca"),
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
        type = "data",
        array = df$err_up,
        arrayminus = df$err_dn,
        color = "#144c73",
        thickness = 1.5),
      legendgroup = "dens")
    
    if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) {
      #    df_cmp$y_off <- species_idx[df_cmp$Species] + offset
      
      p <- p %>% plotly::add_trace(
        data = df_cmp,
        y = ~Species,
        x = ~Mean,
        type = "bar",
        orientation = "h",
        #mode = "markers", ##for scatter plot
        name = cmp_legend,
        showlegend = TRUE,
        marker = base::list(size = 10, color = "#db3b3c"),
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
          type = "data",
          array = df_cmp$err_up,
          arrayminus = df_cmp$err_dn,
          color = "#951b1c",
          thickness = 1.5),
        legendgroup = "dens")}
    p <- p %>% plotly::layout(
      barmode = "group",
      showlegend = TRUE,
      legend = base::list(
        traceorder = "reversed", 
        font = base::list(size = 15),
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = 1, yanchor = "bottom"),
      title = base::list(
        text = DensTitle(),
        font = base::list(size = 22),
        y = 1,
        yanchor = "top",
        pad = base::list(t = 20)),
      xaxis = base::list(
        title = base::list(
          text = densYlabel(),
          font = base::list(size = 18),
          standoff = 20)),
      yaxis = base::list(
        type = "category", categoryorder = "array", categoryarray = species_levels,
        title = base::list(
          text = "Species",
          font = base::list(size = 18),
          standoff = 15),
        ticks = "outside",
        ticklabelposition = "outside"),
      margin = base::list(t = 80, l = 140, r = 40),
      height = fig_height,
      font = base::list(size = 12),
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
  tempDensTableTitle<-shiny::reactive({
    shiny::validate(shiny::need(base::try(base::paste(NPSForVeg::getNames(VEGDATA[input$densPark],"long"),":",densTitleGroup(),densTitleValues(), 
                                                      base::paste0(base::as.character(base::min(densYears())),"-",base::as.character(base::max(densYears())),"(",densYlabel(),")") )), message=FALSE) )
    base::paste(NPSForVeg::getNames(VEGDATA[[input$densPark]],"long"),":",densTitleGroup(),densTitleValues(), 
                base::paste0(base::as.character(base::min(densYears())),"-",base::as.character(base::max(densYears())) ," (",densYlabel(),")") )
  })
  
  output$densTableTitle<-shiny::renderText({ tempDensTableTitle() })  
  
  
  #### Data for Table ####
  DensTableArgs<-shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    veg <- VEGDATA[[input$densPark]]
    shiny::req(veg)
    base::list(
      object = veg,
      group  = input$densGroup,
      years  = densYears(),
      values = input$densvalues,
      area   = base::ifelse(input$densvalues == "size", "ha", "plot"),
      common = base::isTRUE(input$densCommon))})
  
  #### Make Table ####
  
  tempDensTable<-shiny::reactive({
    expr={
      shiny::validate(shiny::need(base::try(
        base::do.call(NPSForVeg::dens, DensTableArgs() )
      ),
      "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."
      ))
      TableOut<-base::do.call(NPSForVeg::dens,DensTableArgs())
      base::names(TableOut)<-base::c("Species",'Mean',"Lower 95% CI", "Upper 95% CI")
      base::return(TableOut)
    }
    
  })
  
  output$densTable<-DT::renderDataTable(tempDensTable())
  
  #### Table Download ####
  
  output$densTableDownload<-shiny::downloadHandler(
    filename=function(){base::paste(tempDensTableTitle(), ".csv", sep="")}, 
    content=function (file){
      utils::write.csv(tempDensTable(),file)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
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
        base::paste0(DATACYCLES$Name, ": ", DATACYCLES$YearStart, "-", DATACYCLES$YearEnd))))})
  
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
    
    raw <- NPSForVeg::IV(
      object = VEGDATA[[input$IVPark]],
      group = input$IVGroup,
      years = IVYears(),
      common = input$IVCommon)
    
    # compute the opposite name for hover text.
    if (base::isTRUE(input$IVCommon)) {
      raw$LabelOpp <- base::tryCatch(
        NPSForVeg::getPlantNames(
          object = VEGDATA[[input$IVPark]],
          names = raw$Species,
          in.style = "common",
          out.style = "Latin"),
        error = function(e) raw$Species)
    } else {
      raw$LabelOpp <- base::tryCatch(
        NPSForVeg::getPlantNames(
          object = VEGDATA[[input$IVPark]],
          names = raw$Species,
          in.style = "Latin",
          out.style = "common"),
        error = function(e) raw$Species)}
    raw$LabelOpp <- base::ifelse(
      base::is.na(raw$LabelOpp) | !base::nzchar(raw$LabelOpp),
      raw$Species,
      raw$LabelOpp)
    
    raw})
  
  ### IV checkbox ###
  iv_text_on <- shiny::reactive(base::isTRUE(input$IVPlotlyText))
  
  # IV Colors
  pickColor <- function(val, fallback) {
    if (base::is.null(val) || base::length(val) == 0 || base::is.na(val) || !base::nzchar(val)) return(fallback)
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
    
    shiny::validate(shiny::need(base::nrow(IVdf) > 0,
        "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."))
    
    # sets number of displayed species
    if (!base::is.na(input$IVTop)) {
      IVdf <- IVdf %>% dplyr::slice_max(order_by = Total, n = input$IVTop, with_ties = FALSE)}
    
    # plot order
    IVdf <- IVdf %>% dplyr::arrange(Total)
    
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
          data = IVdf,
          x = ~Density,
          y = ~Species,
          name = "Density",
          type = "bar",
          orientation = "h",
          marker = base::list(color = IVDensityColor()),
          text = if (iv_text_on()) {~base::sprintf("%.2f", Density)} else {NULL},
          hovertext = ~base::sprintf("Species: %s<br>Density: %.2f", LabelOpp, Density),
          hoverinfo = "text",
          hoverlabel = base::list(
            bgcolor = IVDensityColor(),
            bordercolor = "white",
            font = base::list(size = 14, color = contrastColor(IVDensityColor())))) %>%
        plotly::add_trace(
          data = IVdf,
          x = ~Size,
          y = ~Species,
          name = "Size",
          type = "bar",
          orientation = "h",
          marker = base::list(color = IVSizeColor()),
          text = if (iv_text_on()) {~base::sprintf("%.2f", Size)} else {NULL},
          hovertext = ~base::sprintf("Species: %s<br>Size: %.2f", LabelOpp,Size),
          hoverinfo = "text",
          hoverlabel = base::list(
            bgcolor = IVSizeColor(),
            bordercolor = "white",
            font = base::list(size = 14, color = contrastColor(IVSizeColor())))) %>%
        plotly::add_trace(
          data = IVdf,
          x = ~Distribution,
          y = ~Species,
          name = "Distribution",
          type = "bar",
          orientation = "h",
          marker = base::list(color = IVDistributionColor()),
          text = if (iv_text_on()) {~base::sprintf("%.2f", Distribution)} else {NULL},
          hovertext = ~base::sprintf("Species: %s<br>Distribution: %.2f", LabelOpp, Distribution),
          hoverinfo = "text",
          hoverlabel = base::list(
            bgcolor = IVDistributionColor(),
            bordercolor = "white",
            font = base::list(size = 14, color = contrastColor(IVDistributionColor()))))}
    p <- p %>% plotly::layout(
      barmode = "stack",
      showlegend = TRUE,
      legend = base::list(
        font = base::list(size = 15),
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = 1,   yanchor = "bottom"),
      title = base::list(
        text = IVTitle(),
        font = base::list(size = 22),
        y = 1,
        yanchor = "top",
        pad = base::list(t = 20)),
      xaxis = base::list(
        title = "",
        tickvals = base::c(0, base::max(IVdf$Total)),
        ticktext = base::c("Low", "High"),
        tickfont = base::list(size = 14)),
      yaxis = base::list(
        title = "",
        tickfont = base::list(size = input$IVFontSize),
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
  
  tempIVTable <- shiny::reactive({
    df <- IVData()
    shiny::validate(shiny::need(
      !base::is.null(df) && base::nrow(df) > 0,
      "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."
    ))
    df <- df %>% dplyr::select(-LabelOpp)
    df
  })
  
  output$IVData <- DT::renderDataTable({tempIVTable()})
  #### IV Table download ####
  
  output$IVTableDownload<-shiny::downloadHandler(
    filename=function(){base::paste(tempIVTableTitle(), ".csv", sep="")}, 
    content=function (file){
      utils::write.csv(tempIVTable(),file)
    }
  )
  
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
      shiny::need(input$SpListPark!="")
    )
    shiny::selectizeInput(inputId="SpListPlot", choices=base::c("All Plots"="All", NPSForVeg::getPlotNames(VEGDATA[[input$SpListPark]],type="all")),
                          label="Plots (optional)", multiple=TRUE, selected="All"
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
  
  CommonList<-shiny::reactive(decapitalize(NPSForVeg::getPlantNames(object=VEGDATA[[input$SpListPark]], names=LatinList(), out.style="common",in.style="Latin")))
  
  
  MonitoringList<-shiny::reactive({ 
    tibble::tbl_df(base::data.frame('Latin.Name'=LatinList(),'Common.Name'=CommonList())) %>% 
      dplyr::arrange (Common.Name) %>% 
      dplyr::rename('Latin Name'=Latin.Name, 'Common Name'=Common.Name) %>% 
      .[,c(2,1)]
  })
  
  ###Make URL for and get data from NPSpecies
  NPSpeciesURL<-shiny::reactive({base::paste0("https://irmaservices.nps.gov/v3/rest/npspecies/checklist/",input$SpListPark,"/Vascular%20Plant?format=Json")})
  
  NPSpeciesList<-shiny::reactive({
    jsonlite::fromJSON(NPSpeciesURL()) %>% 
      dplyr::select(CommonNames,ScientificName,Occurrence) %>% 
      dplyr::arrange(CommonNames) %>% 
      dplyr::rename("Latin Name"=ScientificName, "Common Name"=CommonNames)
  })  
  
  
  
  ##Create Title for Table
  
  output$SpeciesTableTitle<- shiny::renderText({
    base::switch(input$SpListType,
                 Monitoring= "Species Found in the Monitoring Plots",
                 NPSpecies="All Species Known from the Park")
  }) 
  
  
  
  ##Create Table 
  output$SpeciesTable<- DT::renderDataTable({
    shiny::validate(
      shiny::need(input$SpListPark!="", message="There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years.
")
    )
    
    DT::datatable(rownames=F, caption="Species List", class="display compact", selection="single",
                  data=base::switch(input$SpListType,
                                    Monitoring= MonitoringList(),
                                    NPSpecies=NPSpeciesList()
                  )
    ) %>% 
      DT::formatStyle('Latin Name', fontStyle='italic' )
  })
  
  
})# end of shiny::shinyServer() function

