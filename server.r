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
                      SHEN=base::list(importSHEN("./Data/SHEN")))

# map park names to 4 letter codes
base::names(VEGDATA)<-NPSForVeg::getNames(VEGDATA, name.class="code")
PARKLIST<-NPSForVeg::getNames(VEGDATA,name.class="code")
base::names(PARKLIST)<-NPSForVeg::getNames(VEGDATA)

# bound parks for zoom
PARKBOUNDS<-utils::read.csv("boundboxes.csv", as.is=TRUE)

# year range into cycles
DATACYCLES<-NPSForVeg::getCycles(VEGDATA[[1]])

# map plant group names to "readable" labels
DENSLABELDATA<-base::data.frame(Name=base::c("trees","saplings","seedlings","shrubs","shseedlings","herbs","vines"), Label=base::c("Trees","Saplings","Tree Seedlings", "Shrubs","Shrub Seedlings","Understory Plants","Vines in Trees"), stringsAsFactors=FALSE)

# read in photos
PLACEHOLDER_PHOTOS <- base::list.files(path = "www/photos/", pattern = "\\.(jpg|jpeg)$", ignore.case = TRUE)

# capitalize first letter in common name, lowercase everything else
fmt_common <- function(x) {base::ifelse(base::is.na(x) | !base::nzchar(base::trimws(x)), x,
                                        base::paste0(base::toupper(base::substr(base::trimws(x), 1, 1)),
                                                     base::tolower(base::substr(base::trimws(x), 2, 
                                                                                base::nchar(base::trimws(x))))))}

# build plotly legend and wrap and scale by screen width
buildPlotlyTitleLegend <- function(title_text, font_size, screen_width, legend_labels = base::character(0), plot_height = 600, smooth_breakpoint = FALSE) {
  sw <- if (!base::is.null(screen_width)) screen_width else 1200
  is_sm <- sw < 768
  
  if (smooth_breakpoint) {
    sm_blend <- base::max(0, base::min(1, (768 - sw) / (768 - 600)))
    title_sz <- base::round((font_size + 10) - (6 * sm_blend))
    legend_sz <- base::round((font_size + 3) - (3 * sm_blend))
    left_m <- base::round(140 - (60 * sm_blend))
  } else {
    title_sz <- if (is_sm) font_size + 4  else font_size + 10
    legend_sz <- if (is_sm) font_size else font_size + 3
    left_m <- if (is_sm) 80 else 140
  }
  
  wrap_w_title <- base::max(24, base::floor((sw * 0.85 - left_m) / (title_sz * 0.6)))
  title_wrapped <- base::gsub("\n", "<br>", stringr::str_wrap(title_text, width = wrap_w_title))
  n_title_lines <- base::length(base::gregexpr("<br>", title_wrapped)[[1]]) + 1
  title_band <- 10 + n_title_lines * (title_sz + 4)
  
  n_entries <- base::max(1, base::length(legend_labels))
  usable_w <- sw * 0.85 - left_m
  
  if (n_entries <= 3) {
    entry_w <- base::max(90, base::floor(usable_w / n_entries))
    entries_per_row <- 1
  } else {
    longest_chars <- base::max(base::nchar(base::gsub("<[^>]+>", "", base::as.character(legend_labels))))
    entry_w <- base::min(usable_w, base::max(70, longest_chars * legend_sz * 0.58 + 30))
    entries_per_row <- base::max(1, base::min(n_entries, base::floor(usable_w / entry_w)))
  }
  
  wrap_w_legend <- base::max(12, base::floor(entry_w / (legend_sz * 0.6)))
  wrapped_legend <- base::lapply(legend_labels, function(lbl) {
    base::gsub("\n", "<br>", stringr::str_wrap(lbl, width = wrap_w_legend))})
  legend_lines <- if (base::length(wrapped_legend) == 0) {
    base::numeric(0)
  } else {
    base::sapply(wrapped_legend, function(lbl) {
      if (base::grepl("<br>", lbl)) base::length(base::gregexpr("<br>", lbl)[[1]]) + 1 else 1})
  }
  max_lines_per_entry <- if (base::length(legend_lines) == 0) 1 else base::max(1, base::max(legend_lines))
  
  n_rows <- base::ceiling(n_entries / entries_per_row)
  leg_rows <- if (base::length(legend_lines) == 0) {
    0
  } else if (n_entries <= 3) {
    base::sum(legend_lines)
  } else {
    n_rows * max_lines_per_entry
  }
  
  narrowness <- base::max(0, base::min(1, (491 - sw) / (491 - 320)))
  row_h <- base::ceiling(legend_sz * 1.55)
  legend_band <- 8 + leg_rows * row_h + (6 * narrowness)
  
  if (n_entries > 3) legend_band <- legend_band * 1.4
  
  gap <- 10 + 8 * narrowness
  top_m <- title_band + legend_band + gap
  
  total_height <- base::max(plot_height, 450 + top_m)
  
  legend_cfg <- base::list(traceorder = "reversed", font = base::list(size = legend_sz),
                           orientation = "h", xref = "paper", x = 0.5, xanchor = "center",
                           yref = "container", y = 1 - (title_band + 10 + legend_band * 0.5) / total_height, yanchor = "middle")
  if (n_entries > 3) {
    legend_cfg$entrywidth <- entry_w
    legend_cfg$entrywidthmode <- "pixels"
  }
  
  base::list(
    title = base::list(text = title_wrapped, font = base::list(size = title_sz),
                       xref = "paper", x = 0.5, xanchor = "center",
                       yref = "container", y = 1 - (title_band * 0.5) / total_height, yanchor = "middle"),
    legend = legend_cfg,
    margin = base::list(t = top_m, l = left_m, r = 40, b = 50),
    legend_labels = wrapped_legend,
    height = total_height)}

# assign random photo with caption to unloaded screens in app
randomPlaceholderImg <- function() {
  if (base::length(PLACEHOLDER_PHOTOS) == 0) base::return(NULL)
  file <- base::sample(PLACEHOLDER_PHOTOS, 1)
  
  # parse "ANTI-0025_20140708_060h.JPG"
  parts <- base::strsplit(file, "_", fixed = TRUE)[[1]]
  caption <- base::tryCatch({
    park_plot <- base::strsplit(parts[1], "-", fixed = TRUE)[[1]]
    park_code <- park_plot[1]
    plot_num <- base::as.integer(park_plot[2])
    park_name <- if (park_code %in% base::names(VEGDATA)) {
      NPSForVeg::getNames(VEGDATA[[park_code]], "long")
    } else { park_code }
    raw_date <- parts[2]
    date_str <- if (base::nchar(raw_date) == 6) {base::format(base::as.Date(base::paste0(raw_date, "01"), "%Y%m%d"), "%B, %Y")
    } else {base::format(base::as.Date(raw_date, "%Y%m%d"), "%B %d, %Y")}
    base::paste0(park_name, ", Plot #", plot_num, " - ", date_str)}, error = function(e) NULL)
  
  htmltools::tags$figure(
    style = "margin:14px auto 0 auto; text-align:center;",
    htmltools::tags$img(
      src = base::paste0("photos/", file),
      style = "display:block; margin:0 auto; max-width:min(95%, 1000px);
               max-height:85vh; height:auto; width:auto; border-radius:8px;"),
    if (!base::is.null(caption)) htmltools::tags$figcaption(
      style = "margin-top:8px; font-size:14px; color:#666; font-style:italic;",
      caption))
}

##### Begin Server Function ####

shiny::shinyServer(function(input,output,session){
  
  # shared: blank-input placeholder images (dens/ts/iv/splists)
  output$densGraphImage <- shiny::renderUI({input$densPark; input$densSpeciesType; input$densSpecies 
    randomPlaceholderImg()})
  
  output$densTableImage <- shiny::renderUI({input$densPark; input$densSpeciesType; input$densSpecies
    randomPlaceholderImg()})
  
  output$tsTableImage <- shiny::renderUI({input$tsPark; input$tsSpeciesType; input$tsSpecies
    randomPlaceholderImg()})
  
  output$tsGraphImage <- shiny::renderUI({input$tsPark; input$tsSpeciesType; input$tsSpecies
    randomPlaceholderImg()})
  
  output$ivGraphImage <- shiny::renderUI({input$IVPark; input$IVSpeciesType; input$IVSpecies
    randomPlaceholderImg()})
  
  output$ivTableImage <- shiny::renderUI({
    input$IVPark; input$IVSpeciesType; input$IVSpecies
    randomPlaceholderImg()})
  
  output$spTableImage <- shiny::renderUI({input$SpListPark; input$SpListType
    randomPlaceholderImg()})
  
  # disable font size sliders on narrow screens
  shiny::observe({
    sw <- input$screenW
    small_screen <- !base::is.null(sw) && sw < 1386
    
    if (small_screen) {
      shiny::updateSliderInput(session, "densFontSize", value = 12)
      shiny::updateSliderInput(session, "tsFontSize", value = 12)
      shiny::updateSliderInput(session, "IVFontSize", value = 12)}
    
    session$sendCustomMessage("toggleSliderDisable", base::list(
      ids = base::c("densFontSize", "tsFontSize", "IVFontSize"),
      disable = small_screen))})
  
  # font size disabled notice
  fontSizeDisabledMsg <- shiny::reactive({
    sw <- input$screenW
    small_screen <- !base::is.null(sw) && sw < 1386
    if (small_screen) {
      htmltools::tags$div(
        style = "font-size: 11px; color: #888; font-style: italic; margin-top: 2px; width: 150px; text-align: center;",
        "Font size adjustment is available on larger screens.")
    } else { NULL }
  })
  
  output$densFontSizeNotice <- shiny::renderUI({ fontSizeDisabledMsg() })
  output$tsFontSizeNotice <- shiny::renderUI({ fontSizeDisabledMsg() })
  output$IVFontSizeNotice <- shiny::renderUI({ fontSizeDisabledMsg() })
  
  shiny::outputOptions(output, "densFontSizeNotice", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "tsFontSizeNotice", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "IVFontSizeNotice", suspendWhenHidden = FALSE)
  
  ####  Map Panel  ####
  
  #### toggles ####
  shiny::observe({
    ### Maps  
    # shinyjs::onclick(id="VideoButton", expr= shinyjs::toggle(id="VideoPanel"))
    # shinyjs::onclick(id="CloseVideo", expr= shinyjs::toggle(id="VideoPanel")) 
    shinyjs::toggle(id='TreeStatus', condition=input$MapGroup=='trees')
  })
  
  
  #### UI Controls ####
  
  #  Park control
  
  output$MapParkControl<-shiny::renderUI({
    shiny::selectizeInput(inputId="MapPark", label="Filter species list by park:",
                          choices = base::c("All Parks" = "All", PARKLIST),
                          selected = NULL,
                          options = base::list(placeholder = "Select a park",
                                               onInitialize = base::I('function() { this.setValue(""); }')))})
  
  # assign data value per group
  ValuesUse<-shiny::reactive({
    base::switch(input$MapGroup,
                 trees=,saplings=base::c(Abundance="count", "Basal Area"="size"),
                 seedlings=,shseedlings=,shrubs=,vines=base::c(Abundance="count"),
                 cwd=base::c("Volume"="size"),
                 herbs=base::c("Percent Cover"="size"))})
  
  # check if value is still valid when group changes
  shiny::observe({shiny::req(ValuesUse())
    current <- input$MapValues
    choices <- unname(ValuesUse())
    
    if (base::is.null(current) || current == "" || !(current %in% choices)) {
      shiny::updateSelectizeInput(session, "MapValues", selected = choices[1])}
  })
  
  output$PlantValueControl<-shiny::renderUI({
    shiny::selectizeInput(inputId="MapValues", label="Data to Map:", choices=if (base::is.null(ValuesUse())) base::character(0) else ValuesUse(), 
                          selected = "", options = base::list(placeholder = "Select a data type",
                                                              onInitialize = base::I('function() { this.setValue(""); }')))})
  
  
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
  
  # establish ecoregion legend/order west to east
  ECOREGION_ORDER <- base::unique(Ecoregion$MapClass)
  
  # Cycles control
  output$MapCycleControl<-shiny::renderUI({
    shiny::req(DATACYCLES)
    shiny::selectizeInput(inputId="MapCycles", label="Display data from years:", 
                          choices=base::rev(stats::setNames(base::as.character(DATACYCLES$Cycle), base::paste0(DATACYCLES$Name,":", DATACYCLES$YearStart,"-",DATACYCLES$YearEnd))),
                          selected = NULL, options = base::list(placeholder = "Select a cycle",
                                                                onInitialize = base::I('function() { this.setValue(""); }')))})
  # cycle ID to years
  MapYears <- shiny::reactive({shiny::req(input$MapCycles)
    year_start <- DATACYCLES %>% dplyr::filter(Cycle == input$MapCycles) %>% dplyr::pull(YearStart)
    year_end <- DATACYCLES %>% dplyr::filter(Cycle == input$MapCycles) %>% dplyr::pull(YearEnd)
    
    available_years <- base::sort(base::unique(NPSForVeg::getEvents(object = VEGDATA, plot.type = "all")$Event_Year))
    
    all_years <- year_start:year_end
    all_years[all_years %in% available_years]
  }) %>% shiny::bindCache(input$MapCycles) 
  
  # Map MetaData
  MapMetaData<-shiny::reactive({
    shiny::req(input$MapValues, input$MapGroup)
    MAPLEGEND[[input$MapValues]][[input$MapGroup]] 
  })
  
  # show all unique plots setting
  AllPlotLocations <- shiny::reactive({
    all_plots <- base::lapply(base::names(VEGDATA), function(park) {
      base::tryCatch(
        NPSForVeg::getPlots(VEGDATA[[park]], output = "dataframe", type = "all") %>%
          dplyr::select(Plot_Name, Unit_Code, Latitude, Longitude),
        error = function(e) NULL)})
    dplyr::bind_rows(all_plots[!base::sapply(all_plots, base::is.null)]) %>%
      dplyr::distinct(Plot_Name, .keep_all = TRUE)})
  
  showAllPlots <- shiny::reactiveVal(TRUE)
  
  # Track whether a reset is in progress
  groupNoData <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(
    base::list(input$MapPark, input$MapCycles),
    {groupNoData(FALSE)},
    ignoreInit = TRUE)
  
  showWarningOverlay <- shiny::reactive({
    groupNoData() || {
      missing_group <- base::is.null(input$MapGroup) || input$MapGroup == ""
      missing_species <- base::is.null(input$MapSpecies) || input$MapSpecies == ""
      missing_values <- base::is.null(input$MapValues) || input$MapValues == ""
      missing_cycle <- base::is.null(input$MapCycles) || input$MapCycles == ""
      missing_status <- base::isTRUE(input$MapGroup == "trees") &&
        (base::is.null(input$TreeStatus) || input$TreeStatus == "")
      base::any(missing_group, missing_species, missing_values, missing_cycle, missing_status)
    }
  })
  
  # Plot years sampled
  PlotYearRanges <- shiny::reactive({
    events <- NPSForVeg::getEvents(object = VEGDATA, plot.type = "all")
    events %>%
      dplyr::group_by(Plot_Name) %>%
      dplyr::summarise(
        FirstYear = base::min(Event_Year, na.rm = TRUE),
        LastYear = base::max(Event_Year, na.rm = TRUE),
        .groups = "drop")
  })
  
  # Plot is retired if its last sampled year predates the most recent cycle
  latestCycleStart <- base::max(DATACYCLES$YearStart)
  
  retiredPlotNote <- function(plot_name) {
    yr <- PlotYearRanges() %>% dplyr::filter(Plot_Name == plot_name)
    if (base::nrow(yr) == 0) base::return("")
    
    cycle_starts <- base::sort(base::unique(DATACYCLES$YearStart))
    
    retirement_threshold <- if (base::length(cycle_starts) >= 2) {
      cycle_starts[base::length(cycle_starts) - 1]
    } else {cycle_starts[base::length(cycle_starts)]}
    
    if (yr$LastYear[1] >= retirement_threshold) base::return("")
    
    last_cycle_row <- DATACYCLES %>%
      dplyr::filter(YearStart <= yr$LastYear[1], YearEnd >= yr$LastYear[1])
    cycle_num <- if (base::nrow(last_cycle_row) > 0) last_cycle_row$Cycle[1] else NA
    
    base::as.character(htmltools::tags$h6(
      style = "color:#b45309; font-style: italic;",
      if (!base::is.na(cycle_num)) {
        base::sprintf("This plot is not actively monitored. Last monitored in %d, Cycle %s.", yr$LastYear[1], cycle_num)
      } else {base::sprintf("This plot is not actively monitored. Last monitored in %d.", yr$LastYear[1])}))
  }
  
  ### if plant type doesnt exist reset placeholder
  shiny::observe({
    shiny::req(input$MapGroup, input$MapCycles, !showAllPlots())
    
    group_slot <- PLANTSLOTLOOKUP[[input$MapGroup]]
    shiny::req(!base::is.null(group_slot))
    
    park_sel <- if (input$MapPark %in% base::c("", "All")) "All" else input$MapPark
    
    has_data <- if (park_sel == "All") {
      base::any(base::sapply(base::names(VEGDATA), function(park) {
        dat <- base::tryCatch(methods::slot(VEGDATA[[park]], group_slot), error = function(e) NULL)
        if (base::is.null(dat) || base::nrow(dat) == 0) base::return(FALSE)
        if (!"Cycle" %in% base::names(dat)) base::return(FALSE)
        base::any(dat$Cycle == base::as.integer(input$MapCycles))
      }))
    } else {
      dat <- base::tryCatch(methods::slot(VEGDATA[[park_sel]], group_slot), error = function(e) NULL)
      if (base::is.null(dat) || base::nrow(dat) == 0) FALSE
      else if (!"Cycle" %in% base::names(dat)) FALSE
      else base::any(dat$Cycle == base::as.integer(input$MapCycles))
    }
    
    if (!has_data) {
      groupNoData(TRUE)
      shiny::updateSelectizeInput(session, "MapGroup", selected = "")
      shiny::updateSelectizeInput(session, "MapSpecies", selected = "")
    } else {
      groupNoData(FALSE)
    }
  })
  
  shiny::observeEvent(
    base::list(
      input$MapGroup,
      input$TreeStatus,
      input$MapValues,
      input$MapCycles,
      input$MapSpecies,
      input$MapPark),
    {
      shiny::req(
        input$MapGroup,
        input$MapValues,
        input$MapCycles,
        input$MapSpecies)
      if (base::isTRUE(input$MapGroup == "trees")) shiny::req(input$TreeStatus)
      
        any_real_input <- !all(c(
          input$MapGroup   %in% base::c("", NULL),
          input$MapValues  %in% base::c("", NULL),
          input$MapCycles  %in% base::c("", NULL),
          input$MapSpecies %in% base::c("", NULL)
        ))
        if (any_real_input) {
          showAllPlots(FALSE)}
      
      # Do nothing if all inputs still match defaults
      default_cycle <- base::as.character(DATACYCLES$Cycle[base::nrow(DATACYCLES)])
      
      is_default <- 
        (base::is.null(input$MapGroup) || base::identical(input$MapGroup, "trees")) &&
        (base::is.null(input$TreeStatus) || base::identical(input$TreeStatus, "alive")) &&
        (base::is.null(input$MapValues) || base::identical(input$MapValues, "count")) &&
        (base::is.null(input$MapCycles) || base::identical(input$MapCycles, default_cycle)) &&
        (base::is.null(input$MapSpecies) || base::identical(input$MapSpecies, "All")) &&
        (base::is.null(input$MapPark) || input$MapPark %in% base::c("", "All"))
      
      if (!is_default) {
        showAllPlots(FALSE)}},
    ignoreInit = TRUE)
  
  # refelcts showallplots()
  output$mapModeIndicator <- shiny::renderUI({
    if (showAllPlots()) {
      htmltools::tags$div(
        style = "padding: 6px 12px; margin-bottom: 8px;
               background-color: #e8f5e9; color: #2e7d32;
               border: 1.5px solid #a5d6a7; border-radius: 6px;
               font-size: 13px; font-weight: bold;",
        "\u25cf  Showing every NCRN plot ever sampled \u2014 no filters below are applied")
    } else {
      shiny::actionButton(
        inputId = "resetMapFilters",
        title = "Click to restore view of all unfiltered monitoring plots",
        label = "\u25cf  Showing filtered plots \u2014 Click here to clear all filters",
        style = "width: 100%; text-align: left;
                 padding: 6px 12px; margin-bottom: 8px;
                 background-color: #e3f2fd; color: #1565c0;
                 border: 1.5px solid #90caf9; border-radius: 6px;
                 font-size: 13px; font-weight: bold;
                 cursor: pointer;")}})
  
  shiny::observeEvent(input$resetMapFilters, {
    
    shiny::updateSelectizeInput(session, "MapGroup", selected = "")
    shiny::updateSelectizeInput(session, "MapValues", selected = "")
    shiny::updateSelectizeInput(session, "MapPark", selected = "")
    shiny::updateSelectizeInput(session, "MapSpecies", selected = "")
    shiny::updateSelectizeInput(session,"TreeStatus", selected = "")
    shiny::updateSelectInput(session, "MapCycles", selected = "")
    showAllPlots(TRUE)
    
    session$sendCustomMessage("resetMapLayers", list())
    
    # zoom back out to the full network, since the MapPark-watching
    # observer intentionally skips empty selections (used to prevent an
    # unwanted zoom on initial page load)
    bounds_row <- PARKBOUNDS[PARKBOUNDS$ParkCode == NETWORK, ]
    if (base::nrow(bounds_row) > 0) {
      leaflet::leafletProxy("VegMap") %>%
        leaflet::fitBounds(
          lng1 = bounds_row$LongW,
          lat1 = bounds_row$LatS,
          lng2 = bounds_row$LongE,
          lat2 = bounds_row$LatN)
    }})
  
  # Data to plot on map - always for all parks 
  
  ### debug ####################################################################
  
  # -- Build plot base once per cycle; join spec data separately --
  PlotBase <- shiny::reactive({
    shiny::req(MapYears(), input$MapGroup)
    park_sel <- if (input$MapPark %in% base::c("", "All")) "All" else input$MapPark
    
    plots <- if (park_sel == "All") {
      NPSForVeg::getPlots(VEGDATA, years=MapYears(), output="dataframe", type="all")
    } else {NPSForVeg::getPlots(VEGDATA[[park_sel]], years=MapYears(), output="dataframe", type="all")}
    
    events <- if (park_sel == "All") {
      NPSForVeg::getEvents(object=VEGDATA, years=MapYears(), plot.type="all")
    } else {NPSForVeg::getEvents(object=VEGDATA[[park_sel]], years=MapYears(), plot.type="all")}
    
    events_deduped <- events %>%
      dplyr::select(Plot_Name, Year=Event_Year) %>%
      dplyr::group_by(Plot_Name) %>%
      dplyr::slice_max(Year, n=1, with_ties=FALSE) %>%
      dplyr::ungroup()
    
    dplyr::inner_join(
      plots %>% dplyr::select(Plot_Name, Unit_Code, Latitude, Longitude),
      events_deduped,
      by="Plot_Name"
    ) %>%
      dplyr::filter(!base::is.na(Latitude) & !base::is.na(Longitude)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Size = if (Unit_Code %in% base::names(VEGDATA)) {
        NPSForVeg::getArea(VEGDATA[[Unit_Code]], group=input$MapGroup)
      } else {
        NA_real_
      }) %>%
      dplyr::ungroup()
  }) %>% shiny::bindCache(MapYears(), input$MapGroup, input$MapPark)
  
  MapData<-shiny::reactive({
    shiny::req(input$MapGroup, input$MapValues, input$MapCycles, input$MapSpecies)
    shiny::req(PlotBase())
    
    if (base::isTRUE(input$MapGroup == "trees")) shiny::req(input$TreeStatus)
    
    shiny::validate(shiny::need(input$MapGroup != "", ""), shiny::need(input$MapValues != "", ""),
                    shiny::need(input$MapCycles != "", ""), shiny::need(input$MapSpecies != "", ""))
    
    shiny::req(input$MapSpecies=="All" | input$MapSpecies %in% NPSForVeg::getPlants(object=VEGDATA, group=input$MapGroup, years=MapYears())$Latin_Name )
    shiny::req(input$MapGroup!="vines" | (input$MapGroup=="vines" & input$MapValues=="count"))
    
    P <- PlotBase()
    
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
    
    status_val <- if (input$MapGroup == "trees") {
      shiny::req(input$TreeStatus)
      
      dplyr::case_when(
        input$TreeStatus == "alive" ~ "alive",
        input$TreeStatus == "snag"  ~ "dead",
        input$TreeStatus == "all"   ~ "all",
        TRUE ~ "alive"
      )
    } else {
      "alive"
    }
    
    species_val <- if (input$MapSpecies == "All") NA else input$MapSpecies
    is_herbs    <- input$MapGroup == "herbs"
    is_tree <- input$MapGroup == "trees"
    park_sel    <- if (input$MapPark %in% base::c("", "All")) "All" else input$MapPark
    
    # -- shared SiteXSpec caller; handles herb/non-herb args difference --
    run_sxs <- function(park_obj) {
      args <- base::list(object=park_obj, group=input$MapGroup,
                         years=MapYears(), species=species_val,
                         values=input$MapValues, plot.type = "all")
      if (is_tree || !is_herbs) { args$status <- status_val; args$area <- "ha" }
      base::tryCatch(base::do.call(NPSForVeg::SiteXSpec, args),
                     error=function(e) {
                       base::message("SiteXSpec failed: ", e$message); NULL })
    }
    
    if (park_sel == "All") {
      results <- base::lapply(base::names(VEGDATA), function(park) run_sxs(VEGDATA[[park]]))
      spec_data <- dplyr::bind_rows(results[!base::sapply(results, base::is.null)])
    } else {
      spec_data <- run_sxs(VEGDATA[[park_sel]])
      shiny::validate(shiny::need(
        !base::is.null(spec_data),
        base::paste("No data found for this species/group/year combination in",
                    NPSForVeg::getNames(VEGDATA[[park_sel]], "long"), ".")
      ))
    }
    
    result <- P %>% dplyr::left_join(spec_data %>% dplyr::select(Plot_Name, Values = Total), by = "Plot_Name")
    
    if (base::isTRUE(input$FilterZeroPlots)) {
      result <- result %>% dplyr::filter(!base::is.na(Values) & Values > 0)
    } else {
      result <- result %>% dplyr::mutate(Values = dplyr::if_else(base::is.na(Values), 0, Values))
    }
    
    base::return(result)
  }) %>% shiny::bindCache(input$MapGroup, input$MapValues, input$MapCycles, input$MapSpecies, input$MapPark, input$TreeStatus, input$FilterZeroPlots) 
  ###########################################################################
  
  
  # Map Colors
  CircleColors<-shiny::reactive({
    shiny::req(MapMetaData()$Cuts)
    shiny::req(!base::is.null(MapData()) && base::nrow(MapData()) > 0)
    
    pos_vals <- MapData()$Values[!base::is.na(MapData()$Values) & MapData()$Values > 0]
    pal <- if (base::length(pos_vals) > 0) {
      leaflet::colorBin(palette=base::c("cyan","magenta4","orangered3"),
                        domain=pos_vals, bins=base::c(MapMetaData()$Cuts+.001))
    } else { function(x) base::rep(ZEROCOLOR, base::length(x)) }
    
    function(x) {
      cols <- base::rep(ZEROCOLOR, base::length(x))
      pos_idx <- !base::is.na(x) & x > 0
      if (base::any(pos_idx)) cols[pos_idx] <- pal(x[pos_idx])
      cols
    }
  }) 
  
  # new palettes that are colorblind-friendly, distinct on light/dark basemaps
  ECOREGION_COLORS <- base::c("#0072B2", "#009E73", "#E69F00", "#CC79A7", "#D55E00") 
  FOREST_COLOR <- "#004488" 
  SOIL_COLORS <- base::c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255")
  ZEROCOLOR <- "#6b6b6b"  # 0/no observation
  
  #### Render Map  ####
  
  output$VegMap <- leaflet::renderLeaflet({
    bounds <- PARKBOUNDS[PARKBOUNDS$ParkCode == NETWORK, ]
    
    leaflet::leaflet(options = leafletOptions(
      scrollWheelZoom = TRUE,  # keep OFF to avoid wheel capturing page scroll
      touchZoom       = FALSE,  # keep OFF to avoid pinch-zoom trapping on mobile
      dragging        = TRUE,   # allow panning
      keyboard        = FALSE,  # optional: prevents keyboard focus hijacking
      tap             = FALSE,   # optional: avoids odd tap delays on mobile
      zoomControl     = FALSE
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
        lng1 = bounds$LongW, lng2 = bounds$LongE, lat1 = bounds$LatS, lat2 = bounds$LatN) %>%
      leaflet::addMapPane("dataLayerPane", zIndex = 350) %>%
      htmlwidgets::onRender(base::paste0(
        base::sprintf("
      function(el, x) {
        var map = this;
          window.vegMap = map;
        
      var NPS_TILE_URLS = {map: '%s', imagery: '%s', light: '%s', slate: '%s'};
      var NPS_PREVIEW_LON = %s, NPS_PREVIEW_LAT = %s;
        ", NPSBASIC, NPSIMAGERY, NPSLIGHT, NPSSLATE,
                      base::mean(base::c(bounds$LongW, bounds$LongE)),
                      base::mean(base::c(bounds$LatS, bounds$LatN))),
        "
        var infoControl = L.control({position: 'bottomright'});
infoControl.onAdd = function(map) {
  var div = L.DomUtil.create('div', 'leaflet-bar map-info-leaflet-control');
  div.innerHTML = '<a href=\"#\" title=\"About the map\" class=\"map-round-icon-btn\" style=\"font-weight:bold;\">?</a>';
  L.DomEvent.disableClickPropagation(div);
  L.DomEvent.on(div, 'click', function(e) {
    L.DomEvent.preventDefault(e);
    $('#mapInfoOverlay').addClass('active');
    $('#mapInfoBox').addClass('active');
  });
  return div;
};
infoControl.addTo(map);

        var plotSizeControl = L.control({position: 'bottomright'});
plotSizeControl.onAdd = function(map) {
var div = L.DomUtil.create('div', 'leaflet-bar plotsize-picker');  div.innerHTML =
    '<div class=\"plotsize-toggle map-round-icon-btn\" title=\"Plot marker size\">' +
    '<span class=\"psdot psdot-sm\"></span>' +
  '<span class=\"psdot psdot-md\"></span>' +
  '<span class=\"psdot psdot-lg\"></span>' +
'</div>' +
    '<div class=\"plotsize-strip\">' +
      '<div class=\"plotsize-label\">Plot Marker Point Size</div>' +
      '<input type=\"range\" class=\"plotsize-slider\" min=\"1\" max=\"10\" step=\"1\" value=\"5\">' +
      '<div class=\"plotsize-endlabel-row\">' +
        '<span class=\"plotsize-endlabel\">1x</span>' +
        '<span class=\"plotsize-endlabel\">10x</span>' +
      '</div>' +
    '</div>';
  L.DomEvent.disableClickPropagation(div);
  L.DomEvent.disableScrollPropagation(div);
  Shiny.setInputValue('PlotSize', 5, {priority: 'event'});
  $(div).on('mouseenter', function() {
    $(div).addClass('plotsize-expanded');
  });
  $(div).on('mouseleave', function() {
    $(div).removeClass('plotsize-expanded');
  });
  $(document).on('click', function(e) {
    if (!$(e.target).closest(div).length) {
      $(div).find('.gmaps-strip').removeClass('open');
    }
  });
  $(div).find('.plotsize-slider').on('input change', function() {
    Shiny.setInputValue('PlotSize', parseInt($(this).val(), 10), {priority: 'event'});
  });
  return div;
};
plotSizeControl.addTo(map);

var opacityControl = L.control({position: 'bottomright'});
opacityControl.onAdd = function(map) {
  var div = L.DomUtil.create('div', 'leaflet-bar opacity-picker');
  div.style.display = 'none';
    div.innerHTML =
    '<div class=\"opacity-toggle map-round-icon-btn\" title=\"Layer transparency\">' +
      '<span class=\"opacity-icon\">&#9682;</span>' +
    '</div>' +
        '<div class=\"opacity-strip\">' +
      '<div class=\"opacity-label\">Layer Trans&shy;par&shy;en&shy;cy</div>' +
      '<input type=\"range\" class=\"opacity-slider\" min=\"0\" max=\"100\" step=\"5\" value=\"50\">' +
      '<div class=\"opacity-endlabel-row\">' +
        '<span class=\"opacity-endlabel\">0%</span>' +
        '<span class=\"opacity-endlabel\">100%</span>' +
      '</div>' +
    '</div>';
  L.DomEvent.disableClickPropagation(div);
  L.DomEvent.disableScrollPropagation(div);
  $(div).on('mouseenter', function() { $(div).addClass('opacity-expanded'); });
  $(div).on('mouseleave', function() { $(div).removeClass('opacity-expanded'); });
  $(div).find('.opacity-slider').on('input change', function() {
    Shiny.setInputValue('LayerOpacity', parseInt($(this).val(), 10), {priority: 'event'});
  });
  return div;
};
opacityControl.addTo(map);
window.opacityControl = opacityControl;

Shiny.addCustomMessageHandler('toggleOpacityControl', function(show) {
  $(window.opacityControl.getContainer()).css('display', show ? 'flex' : 'none');
});

var zeroToggleControl = L.control({position: 'bottomright'});
zeroToggleControl.onAdd = function(map) {
  var div = L.DomUtil.create('div', 'leaflet-bar map-round-icon-btn zero-toggle-btn');
  div.title = 'Hide plots with no observations';
  L.DomEvent.disableClickPropagation(div);

  Shiny.setInputValue('FilterZeroPlots', false, {priority: 'event'});

  $(div).on('click', function() {
    var nowFiltering = !$(div).hasClass('zero-toggle-active');
    $(div).toggleClass('zero-toggle-active');
    div.title = nowFiltering ? 'Show plots with no observations' : 'Hide plots with no observations';
    Shiny.setInputValue('FilterZeroPlots', nowFiltering, {priority: 'event'});
  });
  return div;
};
zeroToggleControl.addTo(map);

L.control.zoom({position: 'bottomright'}).addTo(map);

        // Base tile layers managed directly in JS — guarantees only one is
        // ever visible and that the bottom picker's toggling always works,
        // independent of the R package's internal group-tracking bridge.
                var baseTileLayers = {
          'Map':     L.tileLayer(NPS_TILE_URLS.map, {minZoom: 1}),
          'Imagery': L.tileLayer(NPS_TILE_URLS.imagery, {minZoom: 1}),
          'Light':   L.tileLayer(NPS_TILE_URLS.light, {minZoom: 1}),
          'Slate':   L.tileLayer(NPS_TILE_URLS.slate, {minZoom: 1})
        };
        baseTileLayers['Map'].addTo(map);   // default visible layer
        map.baseTileLayers = baseTileLayers; // stash for the swatch picker below
        
        // Compute one representative tile for preview thumbnails
var previewLon = NPS_PREVIEW_LON, previewLat = NPS_PREVIEW_LAT, previewZoom = 9;
function lonLatToTileXY(lon, lat, zoom) {
  var n = Math.pow(2, zoom);
  var x = Math.floor((lon + 180) / 360 * n);
  var latRad = lat * Math.PI / 180;
  var y = Math.floor((1 - Math.log(Math.tan(latRad) + 1 / Math.cos(latRad)) / Math.PI) / 2 * n);
  return {x: x, y: y, z: zoom};
}
var previewTile = lonLatToTileXY(previewLon, previewLat, previewZoom);
function buildPreviewUrl(template) {
  return template
    .replace('{z}', previewTile.z)
    .replace('{x}', previewTile.x)
    .replace('{y}', previewTile.y);
}
var previewUrls = {
  Map: buildPreviewUrl(NPS_TILE_URLS.map),
  Imagery: buildPreviewUrl(NPS_TILE_URLS.imagery),
  Light: buildPreviewUrl(NPS_TILE_URLS.light),
  Slate: buildPreviewUrl(NPS_TILE_URLS.slate)
};

Shiny.setInputValue('MapLayer', 'None', {priority: 'event'});
      
var filtersControl = L.control({position: 'topleft'}); filtersControl.onAdd = function(map) {
  var div = L.DomUtil.create('div', 'leaflet-bar map-controls-leaflet-control');
  div.innerHTML = '<a href=\"#\" title=\"Map controls\" class=\"map-round-icon-btn\">\u2630</a>';
  L.DomEvent.disableClickPropagation(div);
  L.DomEvent.on(div, 'click', function(e) {
    L.DomEvent.preventDefault(e);
    $('#mapFiltersOverlay').addClass('active');
    $('#mapFiltersSidebar').addClass('active');
  });
  return div;
};
filtersControl.addTo(map);
      
// Top-right: native Leaflet layers control (same icon/position as original),
// repurposed to control Ecoregion / Forested Area / Soil via dummy trigger layers
var noneLayer    = L.layerGroup().addTo(map);  // added first so it shows as the initial checked option
var ecoRegLayer  = L.layerGroup();
var forAreaLayer = L.layerGroup();
var soilLayer    = L.layerGroup();

var ecoBaseLayers = {
  \"None\": noneLayer,
  \"Ecoregion\": ecoRegLayer,
  \"Forested Area\": forAreaLayer,
  \"Soil\": soilLayer
};

var ecoLayersControl = L.control.layers(ecoBaseLayers, null, {position: 'topright', collapsed: true}).addTo(map);

map.on('baselayerchange', function(e) {
  var mapping = {\"None\": \"None\", \"Ecoregion\": \"EcoReg\", \"Forested Area\": \"ForArea\", \"Soil\": \"Soil\"};
  Shiny.setInputValue('MapLayer', mapping[e.name], {priority: 'event'});
});

// Bottom-left: Google-Maps-style base tile picker (Map / Imagery / Light / Slate)
var baseLayerControl = L.control({position: 'bottomleft'});
baseLayerControl.onAdd = function(map) {
  var div = L.DomUtil.create('div', 'gmaps-style-basepicker');
div.innerHTML =
    '<div class=\"gmaps-toggle\" title=\"Map layers\" style=\"background-image:url(' + previewUrls.Imagery + ')\"><span>\u2637 Layers</span></div>' +
    '<div class=\"gmaps-strip\">' +
      '<div class=\"gmaps-swatch active\" data-layer=\"Map\" style=\"background-image:url(' + previewUrls.Map + ')\"><span>Map</span></div>' +
      '<div class=\"gmaps-swatch\" data-layer=\"Imagery\" style=\"background-image:url(' + previewUrls.Imagery + ')\"><span>Imagery</span></div>' +
      '<div class=\"gmaps-swatch\" data-layer=\"Light\" style=\"background-image:url(' + previewUrls.Light + ')\"><span>Light</span></div>' +
      '<div class=\"gmaps-swatch\" data-layer=\"Slate\" style=\"background-image:url(' + previewUrls.Slate + ')\"><span>Slate</span></div>' +
    '</div>';
  L.DomEvent.disableClickPropagation(div);

$(div).on('mouseenter', function() {
    $(div).find('.gmaps-strip').addClass('open');
  });
  $(div).on('mouseleave', function() {
    $(div).find('.gmaps-strip').removeClass('open');
  });
$(div).find('.plotsize-toggle').on('click', function(e) {
    e.stopPropagation();
    $(div).toggleClass('plotsize-expanded');
  });
  $(document).on('click', function(e) {
    if (!$(e.target).closest(div).length) {
      $(div).removeClass('plotsize-expanded');
    }
  });
$(div).find('.gmaps-swatch').on('click', function() {
    var layerName = $(this).data('layer');
    var allBaseLayers = ['Map', 'Imagery', 'Light', 'Slate'];

    allBaseLayers.forEach(function(name) {
      if (name === layerName) {
        if (!map.hasLayer(map.baseTileLayers[name])) map.addLayer(map.baseTileLayers[name]);
      } else {
        if (map.hasLayer(map.baseTileLayers[name])) map.removeLayer(map.baseTileLayers[name]);
      }
    });

    $(div).find('.gmaps-swatch').removeClass('active');
    $(this).addClass('active');
    
    var toggleIconUrl = (layerName === 'Imagery' || layerName === 'Slate') ? previewUrls.Map : previewUrls.Imagery;
    $(div).find('.gmaps-toggle').css('background-image', 'url(' + toggleIconUrl + ')');
  });

  return div;
};
baseLayerControl.addTo(map);
",
"
var labelToggleControl = L.control({position: 'bottomright'});
labelToggleControl.onAdd = function(map) {
  var div = L.DomUtil.create('div', 'leaflet-bar map-round-icon-btn label-toggle-btn');
  div.title = 'Show plot IDs';
  div.innerHTML = 'ID';
  L.DomEvent.disableClickPropagation(div);
  Shiny.setInputValue('showPlotLabels', false, {priority: 'event'});
  $(div).on('click', function() {
    var nowShowing = !$(div).hasClass('label-toggle-active');
    $(div).toggleClass('label-toggle-active');
    div.title = nowShowing ? 'Hide plot names' : 'Show plot names';
    Shiny.setInputValue('showPlotLabels', nowShowing, {priority: 'event'});
  });
  return div;
};
labelToggleControl.addTo(map);

var zeroBtn = document.querySelector('.zero-toggle-btn');
var labelBtn = document.querySelector('.label-toggle-btn');
if (zeroBtn && labelBtn) {
  var zeroContainer = zeroBtn.closest('.leaflet-control');
  var labelContainer = labelBtn.closest('.leaflet-control');
  if (zeroContainer && labelContainer) {
    zeroContainer.parentNode.insertBefore(labelContainer, zeroContainer.nextSibling);
  }
}
}
"))
  })
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
      input$MapLayer
      
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
          fillOpacity = 1
        )
      
    } else {
      if (showWarningOverlay()) {
        leaflet::leafletProxy("VegMap") %>%
          leaflet::clearGroup("Circles")
        base::return()}      
      
      md <- MapData()  # single call; reuse local var to avoid re-evaluation
      shiny::req(!base::is.null(md) && base::nrow(md) > 0)
      input$MapLayer  #make sure Circles are always on top
      cols <- CircleColors()(md$Values)
      
      leaflet::leafletProxy("VegMap") %>%
        leaflet::clearGroup("Circles") %>%
        leaflet::addCircles(data=md, radius=15*base::as.numeric(input$PlotSize), group="Circles",
                            lng=md$Longitude, lat=md$Latitude,
                            layerId=md$Plot_Name,  #This is the ID of the circle to match to other data
                            fillColor=cols,
                            color=cols,
                            fillOpacity=1
        )
    }
  })
  
  # Add Circle legends 
  shiny::observe({
    if (showAllPlots()) {
      leaflet::leafletProxy("VegMap") %>%
        leaflet::removeControl(layerId = "CircleLegend")
    } else {
      meta <- MapMetaData()
      shiny::req(base::is.list(meta))
      labels <- base::as.character(meta$Labels)
      shiny::req(base::is.vector(labels))
      
      # Drop zero bins from labels
      zero_idx <- base::which(labels %in% base::c("0", "0%"))
      if (base::length(zero_idx) > 0) {
        labels <- labels[-zero_idx]}
      
      # Use same number of colors as visible labels
      all_colors <- BLUEOR(base::length(labels))
      
      if (!base::isTRUE(input$FilterZeroPlots)) {
        labels <- base::c("0", labels)
        all_colors <- base::c(ZEROCOLOR, all_colors)}
      
      leaflet::leafletProxy("VegMap") %>%
        leaflet::removeControl(layerId = "CircleLegend") %>%
        leaflet::addLegend(
          title   = meta$Title,
          colors  = all_colors,
          labels  = labels,
          layerId = "CircleLegend",
          opacity = 1)}
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

    actual_slot_name <- PLANTSLOTLOOKUP[[input$MapGroup]]
    shiny::validate(
      shiny::need(!base::is.null(actual_slot_name), "Selected plant group is not available in this network")
    )
    
    if (input$MapPark %in% base::c("", "All")) {
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
  
  # Cache the expensive filled lookup so ITIS is only called once per session
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
    shiny::req(input$MapGroup, input$MapCycles)
    shiny::req(base::length(MapYears()) > 0)
    park_sel <- if (input$MapPark %in% base::c("", "All")) "All" else input$MapPark
    SpecTemp<-base::unique(NPSForVeg::getPlants(object=if(park_sel=="All") {VEGDATA}  else {VEGDATA[[park_sel]]} , group=input$MapGroup,
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
    base::return(SpecTemp)
  }) %>% shiny::bindCache(input$MapPark, input$MapGroup, MapYears(), input$mapCommon)
  
  # Species control
  output$MapSpeciesControl <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "MapSpecies",
      label = "Select a species present in selected park:",
      choices = NULL,
      selected = NULL,
      options = base::list(
        placeholder = "Select a species"))})
  
  shiny::observe({shiny::req(input$MapGroup, input$MapCycles)
    shiny::updateSelectizeInput(
      session = session,
      inputId = "MapSpecies",
      choices = MapSpecList(),
      selected = input$MapSpecies)})
  
  # Add GeoJSON polygon layer 
  
  shiny::observe({shiny::req(input$MapLayer)
    leaflet::leafletProxy("VegMap") %>% {
      base::switch(input$MapLayer,
                   None=leaflet::clearGroup(.,group=base::c("Ecoregion","Forested","Soil")) %>% leaflet::removeControl(.,"LayerLegend"),
                   
                   EcoReg=leaflet::clearGroup(.,group=base::c("Forested","Soil") )%>% 
                     leaflet::addPolygons(., data=Ecoregion, group="Ecoregion", layerId=Ecoregion$MapClass, 
                                          stroke=FALSE, options = leaflet::pathOptions(pane = "dataLayerPane"),
                                          fillOpacity=.4, color=leaflet::colorFactor(palette=ECOREGION_COLORS, levels=ECOREGION_ORDER)(Ecoregion$MapClass),
                                          label=base::paste0("Ecoregion: ", Ecoregion$MapClass),
                                          popup=base::paste0("<b>Ecoregion:</b> ", Ecoregion$MapClass)),
                   
                   ForArea=leaflet::clearGroup(.,group=base::c("Ecoregion","Soil")) %>% 
                     leaflet::addPolygons(.,data=Forested, group="Forested", layerId=Forested$MapClass, stroke=FALSE, options = leaflet::pathOptions(pane = "dataLayerPane"),
                                          fillOpacity=.4, color=FOREST_COLOR,
                                          label=base::paste0("Forested: ", Forested$MapClass),
                                          popup=base::paste0("<b>Forested:</b> ", Forested$MapClass)),
                   
                   Soil=leaflet::clearGroup(.,group=base::c("Ecoregion","Forested")) %>% 
                     leaflet::addPolygons(.,data=Soil, group="Soil", layerId=Soil$MapClass, stroke=FALSE, options = leaflet::pathOptions(pane = "dataLayerPane"),
                                          fillOpacity=.4, color=leaflet::colorFactor(SOIL_COLORS,levels=Soil$MapClass)(Soil$MapClass),
                                          label=base::paste0("Soil: ", Soil$MapClass),
                                          popup=base::paste0("<b>Soil:</b> ", Soil$MapClass))
      )}
  })
  
  # Show/hide transparency slider only when a data layer is active
  
  shiny::observeEvent(input$MapLayer, {
    session$sendCustomMessage("toggleOpacityControl", input$MapLayer != "None")
  })
  
  # Apply transparency slider to the active data layer -debounce
  LayerOpacityDebounced <- shiny::debounce(shiny::reactive(input$LayerOpacity), millis = 150)
  
  # Apply transparency slider to the active data layer
  shiny::observe({
    shiny::req(input$MapLayer, input$MapLayer != "None", LayerOpacityDebounced())
    op <- LayerOpacityDebounced() / 100
    leaflet::leafletProxy("VegMap") %>% {
      base::switch(input$MapLayer,
                   EcoReg = leaflet::addPolygons(., data=Ecoregion, group="Ecoregion", layerId=Ecoregion$MapClass, stroke=FALSE, options = leaflet::pathOptions(pane = "dataLayerPane"),
                                                 fillOpacity=op, color=leaflet::colorFactor(palette=ECOREGION_COLORS, levels=ECOREGION_ORDER)(Ecoregion$MapClass)),
                   ForArea = leaflet::addPolygons(., data=Forested, group="Forested", layerId=Forested$MapClass, stroke=FALSE, options = leaflet::pathOptions(pane = "dataLayerPane"),
                                                  fillOpacity=op, color=FOREST_COLOR),
                   Soil = leaflet::addPolygons(., data=Soil, group="Soil", layerId=Soil$MapClass, stroke=FALSE, options = leaflet::pathOptions(pane = "dataLayerPane"),
                                               fillOpacity=op, color=leaflet::colorFactor(SOIL_COLORS,levels=Soil$MapClass)(Soil$MapClass))
      )}
  })
  
  # show plot numbers toggle
  shiny::observe({
    if (!isTRUE(input$showPlotLabels)) {
      leaflet::leafletProxy("VegMap") %>%
        leaflet::clearGroup("PlotLabels")
      return()
    }
    
    if (showAllPlots()) {
      lbl_df <- AllPlotLocations()
    } else {
      if (showWarningOverlay()) {
        leaflet::leafletProxy("VegMap") %>%
          leaflet::clearGroup("PlotLabels")
        return()
      }
      lbl_df <- MapData()
    }
    
    shiny::req(!is.null(lbl_df) && nrow(lbl_df) > 0)
    
    leaflet::leafletProxy("VegMap") %>%
      leaflet::clearGroup("PlotLabels") %>%
      leaflet::addLabelOnlyMarkers(
        data = lbl_df,
        lng = lbl_df$Longitude,
        lat = lbl_df$Latitude,
        label = lbl_df$Plot_Name,
        group = "PlotLabels",
        labelOptions = leaflet::labelOptions(
          noHide = TRUE, direction = "center", textOnly = TRUE,
          className = "plot-name-label",
          style = list("font-weight" = "bold", "font-size" = "11px", "color" = "#222",
                       "text-shadow" = "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff")))
  })
  
  # Zoom the map
  
  # Zoom map when park filter changes
  
shiny::observeEvent(input$MapPark, {
  shiny::req(input$MapPark, input$MapPark != "")
    zoom_target <- if (!base::is.null(input$MapPark) && input$MapPark != "" && input$MapPark != "All") {input$MapPark
    } else {NETWORK}
    
    bounds_row <- PARKBOUNDS[PARKBOUNDS$ParkCode == zoom_target, ]
    
    shiny::req(base::nrow(bounds_row) > 0)
    
    leaflet::leafletProxy("VegMap") %>%
      leaflet::fitBounds(
        lng1 = bounds_row$LongW,
        lat1 = bounds_row$LatS,
        lng2 = bounds_row$LongE,
        lat2 = bounds_row$LatN)
  }, ignoreInit = TRUE)

  # Add layer legends 
  
  shiny::observe({shiny::req(input$MapLayer)
    leaflet::leafletProxy("VegMap") %>%  leaflet::removeControl(layerId="LayerLegend") %>%
      { base::switch(input$MapLayer,
                     None=NA,
                     EcoReg= leaflet::addLegend(.,title="Layer Legend",pal=leaflet::colorFactor(ECOREGION_COLORS, levels=ECOREGION_ORDER),
                                                values=base::factor(ECOREGION_ORDER, levels=ECOREGION_ORDER), layerId="LayerLegend"),
                     ForArea= leaflet::addLegend(.,title="Layer Legend",colors=FOREST_COLOR,
                                                 labels=base::unique(Forested$MapClass), layerId="LayerLegend"),
                     Soil= leaflet::addLegend(.,title="Layer Legend",pal=leaflet::colorFactor(SOIL_COLORS, levels=Soil$MapClass),
                                              values=Soil$MapClass, layerId="LayerLegend", className="info legend soil-legend")
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
            retiredPlotNote(selectedPlot$Plot_Name),
            shiny::h6("Monitoring Plot: ", selectedPlot$Plot_Name),
            htmltools::tags$h6("Use filters to see data for this plot")
          )
        )
      base::return()
    }
    
    md <- MapData()
    selectedPlot <- md[md$Plot_Name == ShapeOver$id, ]
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
                         retiredPlotNote(selectedPlot$Plot_Name),
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
            retiredPlotNote(selectedPlot$Plot_Name),
            shiny::h6("Monitoring Plot: ", selectedPlot$Plot_Name),
            htmltools::tags$h6("Use the filters to see species data for this plot")
          )
        )
      base::return()
    }
    
    md <- MapData()
    selectedPlot <- md[md$Plot_Name == ShapeClick$id, ]
    if (base::nrow(selectedPlot) == 0) base::return()
    
    sxs_args_base <- base::list(
      object  = VEGDATA[[selectedPlot$Unit_Code]],
      group   = input$MapGroup,
      years   = selectedPlot$Year,
      plots   = ShapeClick$id,
      common  = input$mapCommon,
      status  = if (input$MapGroup == "trees") input$TreeStatus else "alive",
      plot.type = "all")
    
    if (base::class(base::try(
      base::do.call(NPSForVeg::SiteXSpec, sxs_args_base), silent = TRUE)) == "try-error") {
      content <- base::as.character(htmltools::tagList(
        htmltools::tags$h6("None found on this plot")))
    } else {
      tempData <- if (input$MapGroup != "herbs") {
        base::do.call(NPSForVeg::SiteXSpec,
                      base::c(sxs_args_base, base::list(values=input$MapValues, area="ha")))[-1]
      } else {
        base::do.call(NPSForVeg::SiteXSpec,
                      base::list(object=VEGDATA[[selectedPlot$Unit_Code]], group=input$MapGroup,
                                 years=selectedPlot$Year, plots=ShapeClick$id,
                                 values=input$MapValues, common=input$mapCommon, plot.type = "all"))[-1]
      }
      base::names(tempData) <- fmt_common(base::names(tempData))
      content <- base::paste0(
        shiny::h5(NPSForVeg::getNames(VEGDATA[[selectedPlot$Unit_Code]], "long")),
        retiredPlotNote(selectedPlot$Plot_Name),
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
                     Circles   = leaflet::addPopups(map = ., lat = ShapeClick$lat + .001, lng = ShapeClick$lng, layerId = "CircleClickPopup", popup = content)
        )
      }
  })

  
  #Build warning labels
  ########## filter warning messages no longer in use with 0  plots toggle ##########
  
  # disableWarnings <- shiny::reactive({
  #   species <- input$MapSpecies
  #   park <- input$MapPark
  #   base::isTRUE(species == "All") && base::isTRUE(park %in% base::c("", "All"))})
  
  # plotCounts <- shiny::reactive({
  #   all_plots <- base::lapply(base::names(VEGDATA), function(park) {
  #     base::tryCatch(
  #       NPSForVeg::getPlots(VEGDATA[[park]], output = "dataframe", type = "all") %>%
  #         dplyr::select(Plot_Name, Unit_Code, Latitude, Longitude), error = function(e) NULL)})
  #   
  #   all_plots <- dplyr::bind_rows(all_plots[!base::sapply(all_plots, base::is.null)]) %>%
  #     dplyr::distinct(Plot_Name, .keep_all = TRUE)
  #   
  #   total <- base::nrow(all_plots)
  #   filtered <- base::length(base::unique(MapData()$Plot_Name))
  #   
  #   base::list(total = total, filtered = filtered, removed = total - filtered)
  # })
  
  # helpers
  # clearWarnings <- function() {
  #   customNotificationMsg(NULL)}
  # 
  # last_park <- shiny::reactiveVal(NULL)
  # last_species <- shiny::reactiveVal(NULL)
  # last_cycle <- shiny::reactiveVal(NULL)
    lastValidGroup <- shiny::reactiveVal("")
  
  # park warning: when plotCounts() or MapPark changes
  # shiny::observe({
  #   if (base::isTRUE(disableWarnings()) || showAllPlots()) {
  #     clearWarnings()
  #     last_park(NULL)
  #     last_species(NULL)
  #     base::return()}
  #   
  #   # park warning only relevant when viewing all species
  #   if (!base::isTRUE(input$MapSpecies == "All")) {
  #     shiny::removeNotification(id = "park_warning")
  #     last_park(NULL)
  #     base::return()}
  #   
  #   shiny::req(MapYears(), input$MapPark, input$MapPark != "")
  #   shiny::req(plotCounts())
  #   
  #   pc <- plotCounts()
  #   
  #   group_label <- dplyr::case_when(
  #     input$MapGroup == "trees" ~ "tree",
  #     input$MapGroup == "shrubs" ~ "shrub",
  #     input$MapGroup == "saplings" ~ "sapling",
  #     input$MapGroup == "seedlings" ~ "seedling",
  #     input$MapGroup == "shseedlings" ~ "shrub seedling",
  #     input$MapGroup == "vines" ~ "vine",
  #     input$MapGroup == "herbs" ~ "understory plant",
  #     TRUE ~ input$MapGroup)
  #   
  #   msg <- base::paste0("Warning: ", pc$removed, " of 430 plots have been removed from the map. ",
  #                       "Map shows plots in the selected park sampled during the selected cycle with at least one recorded ",
  #                       group_label, " observation.")
  #   
  #   if (!base::identical(last_park(), msg)) {
  #     last_park(msg)
  #     customNotificationMsg(msg)}})
  
  # species warning: when MapSpecies or plotCounts() changes
  # shiny::observe({
  #   if (base::isTRUE(disableWarnings()) || showAllPlots()) {
  #     clearWarnings()
  #     last_park(NULL)
  #     last_species(NULL)
  #     base::return()}
  #   
  #   if (base::isTRUE(input$MapSpecies == "All") || base::isTRUE(input$MapSpecies == "")) {
  #     shiny::removeNotification(id = "species_warning")
  #     last_species(NULL)
  #     base::return()}
  #   
  #   shiny::req(input$MapSpecies, input$MapSpecies != "", input$MapSpecies != "All")
  #   shiny::req(MapData(), MapYears())
  #   
  #   pc <- plotCounts()
  #   spec_list <- MapSpecList()
  #   
  #   species_name <- if (input$MapSpecies %in% spec_list) {
  #     base::names(spec_list)[spec_list == input$MapSpecies]
  #   } else {input$MapSpecies}
  #   
  #   filtered_n <- pc$filtered
  #   removed_n  <- pc$removed
  #   filtered_plot_word <- if (filtered_n == 1) "plot" else "plots"
  #   removed_plot_word  <- if (removed_n == 1) "plot" else "plots"
  #   removed_verb <- if (removed_n == 1) "was" else "were"
  #   location_word <- if (removed_n == 1) "this plot" else "these plots"
  #   status_label <- dplyr::case_when(input$TreeStatus == "all"  ~ "", input$TreeStatus == "snag" ~ "dead", TRUE ~ input$TreeStatus)
  # 
  #   msg <- base::paste0("Warning: ", species_name, " was observed at ", filtered_n, " ", filtered_plot_word,
  #                       " under the current filters. ", removed_n, " of 430 plots ", removed_verb, 
  #                       " removed because NCRN has no recorded ", status_label, " ", species_name, " observations for ", 
  #                       input$MapGroup, " during the selected cycle at ", location_word, ".")
  #   
  #   
  #   if (!base::identical(last_species(), msg)) {
  #     last_species(msg)
  #     customNotificationMsg(msg)}})
  
  # cycle warning: only when all parks and all species selected
  # shiny::observe({
  #   shiny::req(input$MapCycles, input$MapSpecies, input$MapPark)
  #   
  #   if (!base::isTRUE(disableWarnings()) || showAllPlots()) {
  #     shiny::removeNotification(id = "cycle_warning")
  #     last_cycle(NULL)
  #     base::return()}
  #   
  #   pc <- plotCounts()
  #   
  #   group_label <- dplyr::case_when(
  #     input$MapGroup == "trees" ~ "tree",
  #     input$MapGroup == "shrubs" ~ "shrub",
  #     input$MapGroup == "saplings" ~ "sapling",
  #     input$MapGroup == "seedlings" ~ "seedling",
  #     input$MapGroup == "shseedlings" ~ "shrub seedling",
  #     input$MapGroup == "vines" ~ "vine",
  #     input$MapGroup == "herbs" ~ "understory plant",
  #     TRUE ~ input$MapGroup)
  #   
  #   status_label <- dplyr::case_when(input$TreeStatus == "all" ~ "", input$TreeStatus == "alive" ~ "living", 
  #                                    input$TreeStatus == "snag"  ~ "dead", TRUE ~ input$TreeStatus)
  #   
  #   # no warning if nothing removed
  #   if (pc$removed <= 0) {
  #     shiny::removeNotification(id = "cycle_warning")
  #     last_cycle(NULL)
  #     base::return()}
  #   
  #   msg <- base::paste0("Warning: ", pc$removed, " of 430 plots have been removed from the map. ",
  #                       "Plots are only shown if they were sampled during the selected cycle and have at least one recorded ",
  #                       status_label, " ", group_label, " observation under the current filters.")
  #   
  #   if (!base::identical(last_cycle(), msg)) {
  #     last_cycle(msg)
  #     customNotificationMsg(msg)}})
  
  shiny::observe({
    if (!base::is.null(input$MapGroup) && input$MapGroup != "") {
      lastValidGroup(input$MapGroup)}})
  
  # clear all popups when navigating away from map page
  # shiny::observeEvent(input$MainNavBar, {
  #   if (!base::isTRUE(input$MainNavBar == "Map")) {
  #     customNotificationMsg(NULL)}})
  
  # clear the blue notification whenever the center warning is shown
  # shiny::observe({
  #   if (!showAllPlots() && showWarningOverlay()) {
  #     customNotificationMsg(NULL)
  #   }
  # })
    
  output$incompleteInputWarning <- shiny::renderUI({
    if (showAllPlots()) base::return(NULL)
    if (!showWarningOverlay()) base::return(NULL)
    
    # Build reactive message
    msg <- if (groupNoData()) {
      group_label <- switch(lastValidGroup(),
                            trees        = "tree",
                            saplings     = "sapling",
                            seedlings    = "tree seedling",
                            shrubs       = "shrub",
                            shseedlings  = "shrub seedling",
                            herbs        = "understory plant",
                            vines        = "vine",
                            input$MapGroup)
      
      park_label <- if (base::is.null(input$MapPark) || input$MapPark %in%base::c("", "All")) {
        "any monitored park"
      } else { NPSForVeg::getNames(VEGDATA[[input$MapPark]], "long") }

      
      cycle_label <- if (!base::is.null(input$MapCycles) && input$MapCycles != "") {
        base::paste0("Cycle ", input$MapCycles)
      } else "the selected cycle"
      
      base::paste0("\u26a0  There are no ", group_label, " observations recorded at ",
                   park_label, " during ", cycle_label,
                   ". Please select a different plant group, species, or park.")
    } else {
      # Incomplete inputs — figure out what's missing
      missing <- base::c()
      if (base::is.null(input$MapGroup)   || input$MapGroup == "")   missing <-base::c(missing, "Plant Group")
      if (base::is.null(input$MapValues)  || input$MapValues == "")  missing <-base::c(missing, "Data to Map")
      if (base::is.null(input$MapCycles)  || input$MapCycles == "")  missing <-base::c(missing, "Monitoring Cycle")
      if (base::is.null(input$MapSpecies) || input$MapSpecies == "") missing <-base::c(missing, "Species")
      if (base::isTRUE(input$MapGroup == "trees") && 
          (base::is.null(input$TreeStatus) || input$TreeStatus == "")) missing <-base::c(missing, "Tree Status")
      
      base::paste0("\u26a0  Please select: ", base::paste(missing, collapse = ", "), ".")
    }
    
    htmltools::tags$div(
      id = "incompleteInputWarningBox",
      style = "background-color: rgba(0, 0, 0, 0.6);
             color: white;
             padding: 16px 24px;
             border-radius: 10px;
             font-size: 15px;
             font-weight: bold;
             text-align: center;
             pointer-events: none;
             max-width: 420px;
             line-height: 1.5;",
      msg)
  })
  
  #### map notification/warning formatting ####
  # customNotificationMsg <- shiny::reactiveVal(NULL)
  # 
  # output$customMapNotification <- shiny::renderUI({
  #   msg <- customNotificationMsg()
  #   shiny::req(msg)
  #   htmltools::tags$div(
  #     id = "customMapNotificationBox",
  #     style = "background-color: #d9edf7;
  #            color: #31708f;
  #            padding: 10px 26px 10px 12px;
  #            border-radius: 4px;
  #            font-size: 12px;
  #            font-weight: normal;
  #            text-align: left;
  #            line-height: 1.4;
  #            box-shadow: 0 1px 6px rgba(0,0,0,0.2);
  #            border: 1px solid #bce8f1;
  #            position: relative;",
  #     htmltools::tags$span(
  #       style = "position:absolute; top:7px; right:9px; cursor:pointer; font-size:13px; color:#31708f; opacity:0.7;",
  #       onclick = "Shiny.setInputValue('dismiss_custom_notification', Math.random())",
  #       "\u00d7"),
  #     msg)
  # })
  # 
  # shiny::observeEvent(input$dismiss_custom_notification, {
  #   customNotificationMsg(NULL)
  # })
  
  #### Force map control outputs to render even while the panel is hidden ####
  shiny::outputOptions(output, "mapModeIndicator", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "MapParkControl", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "MapCycleControl", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "MapSpeciesControl", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "PlantValueControl", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "incompleteInputWarning", suspendWhenHidden = FALSE)
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
                 Least= {max_sp <- densSpeciesCount()
                 htmltools::tags$div(title="Select the maximum number of species to plot", 
                                     shiny::sliderInput(inputId="densTop",label="Maximum number of species to plot (least common, in order of mean value):",
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
                                           shiny::selectizeInput(inputId="ComparePark",choices=c("All Parks" = "ALL", PARKLIST), label="Park:", selected = "ALL")),
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
  
  # dens reset buttons
  shiny::observeEvent(input$densResetData, {
    shiny::updateSelectizeInput(session, "densPark", selected = "")
    shiny::updateSelectInput(session, "densCycles",
                             selected = base::as.character(DATACYCLES$Cycle[base::nrow(DATACYCLES)]))
    shiny::updateSelectizeInput(session, "densGroup", selected = PLANTTYPES[[1]])
    shiny::updateCheckboxInput(session, "densCommon", value = TRUE)
    shiny::updateCheckboxInput(session, "plotlyText", value = FALSE)
    shiny::updateRadioButtons(session, "densSpeciesType", selected = "Common")
    shiny::updateSelectizeInput(session, "densSpecies", selected = base::character(0))
    shiny::updateRadioButtons(session, "CompareType", selected = "None")
  })
  
  shiny::observeEvent(input$densResetDisplay, {
    shiny::updateSelectizeInput(session, "densBaseColor", selected = "blue")
    shiny::updateSelectizeInput(session, "densCompareColor", selected = "red")
    shiny::updateSliderInput(session, "densErrorThickness", value = 1.5)
    shiny::updateSliderInput(session, "densFontSize", value = 12)
  })
  
  densDataIsDefault <- shiny::reactive({
    default_cycle <- base::as.character(DATACYCLES$Cycle[base::nrow(DATACYCLES)])
    
    (base::is.null(input$densPark)        || base::identical(input$densPark, "")) &&
      (base::is.null(input$densCycles)      || base::identical(input$densCycles, default_cycle)) &&
      (base::is.null(input$densGroup)       || base::identical(input$densGroup, PLANTTYPES[[1]])) &&
      (base::is.null(input$densCommon)      || base::isTRUE(input$densCommon)) &&
      (base::is.null(input$plotlyText)      || !base::isTRUE(input$plotlyText)) &&
      (base::is.null(input$densSpeciesType) || base::identical(input$densSpeciesType, "Common")) &&
      (base::is.null(input$densSpecies)     || base::length(input$densSpecies) == 0) &&
      (base::is.null(input$CompareType)     || base::identical(input$CompareType, "None"))
  })
  
  shiny::observe({
    shinyjs::toggleClass(id = "densResetData", class = "btn-danger",
                         condition = !densDataIsDefault())
  })
  
  densDisplayIsDefault <- shiny::reactive({
    (base::is.null(input$densBaseColor)      || base::identical(input$densBaseColor, "blue")) &&
      (base::is.null(input$densCompareColor)   || base::identical(input$densCompareColor, "red")) &&
      (base::is.null(input$densErrorThickness) || input$densErrorThickness == 1.5) &&
      (base::is.null(input$densFontSize)       || input$densFontSize == 12)
  })
  
  shiny::observe({
    shinyjs::toggleClass(id = "densResetDisplay", class = "btn-danger",
                         condition = !densDisplayIsDefault())
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
                       area = if (input$densvalues == "presab") "plot" else "ha"))}
                   base::return(list(
                     object = VEGDATA[[input$ComparePark]],
                     park   = input$ComparePark,
                     group  = input$densGroup,
                     years  = densYears(),
                     values = input$densvalues,
                     common = input$densCommon,
                     area = if (input$densvalues == "presab") "plot" else "ha"))},
                 "Growth Stage"=base::return(base::list(object=VEGDATA[[input$densPark]], group=input$CompareGroup, years=densYears(),
                                                        values=input$densvalues,
                                                        common=input$densCommon,area= if (input$densvalues == "presab") "plot" else "ha")),
                 Time=base::return(base::list(object=VEGDATA[[input$densPark]], group=input$densGroup, years=compYears(),
                                              values=input$densvalues, 
                                              common=input$densCommon,area= if (input$densvalues == "presab") "plot" else "ha")))
  })
  
  #### disable compare color picker when no comparison is selected ####
  shiny::observe({shinyjs::toggleState(id = "densCompareColor", condition = !base::identical(input$CompareType, "None"))})
  
  #### compare color disabled message ####
  compareColorDisabledMsg <- shiny::reactive({
    if (base::identical(input$CompareType, "None")) {
      htmltools::tags$div(style = "font-size: 11px; color: #888; font-style: italic; margin-top: 2px; width: 150px; text-align: center;",
                          "Comparison color is available when a comparison is selected.")
    } else { NULL }})
  
  output$densCompareColorNotice <- shiny::renderUI({ compareColorDisabledMsg() })
  
  shiny::outputOptions(output, "densCompareColorNotice", suspendWhenHidden = FALSE)
  
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
  densOnePlotWarningDismissed <- shiny::reactiveVal(FALSE)
  
  shiny::observeEvent(base::list(input$densPark, input$densGroup, input$densCycles, input$densvalues), {densOnePlotWarningDismissed(FALSE)})
  
  shiny::observeEvent(input$dismiss_dens_onePlot_warning, {densOnePlotWarningDismissed(TRUE)})
  
  output$densOnePlotWarningGraph <- shiny::renderUI({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    if (densOnePlotWarningDismissed()) base::return(NULL)
    veg <- VEGDATA[[input$densPark]]
    shiny::req(veg)
    n_plots <- base::tryCatch(
      base::nrow(NPSForVeg::getPlots(veg, years = densYears(), type = "all")),
      error = function(e) NA_integer_)
    if (base::is.na(n_plots) || n_plots >= 2) base::return(NULL)
    htmltools::tags$div(style = "padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
                        "Warning: Only one monitoring plot exists for this park and cycle. Confidence intervals cannot be estimated.",
                        htmltools::tags$button("\u00d7", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                                               onclick = "Shiny.setInputValue('dismiss_dens_onePlot_warning', Math.random())"))})
  
  output$densOnePlotWarningTable <- shiny::renderUI({shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    if (densOnePlotWarningDismissed()) base::return(NULL)
    veg <- VEGDATA[[input$densPark]]
    shiny::req(veg)
    n_plots <- base::tryCatch(base::nrow(NPSForVeg::getPlots(veg, years = densYears(), type = "all")), error = function(e) NA_integer_)
    if (base::is.na(n_plots) || n_plots >= 2) base::return(NULL)
    htmltools::tags$div(style = "padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
                        "Warning: Only one monitoring plot exists for this park and cycle. Confidence intervals cannot be estimated.",
                        htmltools::tags$button("\u00d7", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                                               onclick = "Shiny.setInputValue('dismiss_dens_onePlot_warning', Math.random())"))})
  
  ##############bugfix
  densData <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    veg <- VEGDATA[[input$densPark]]
    shiny::req(veg)
    
    if (input$densvalues == "size" && input$densGroup %in% base::c("vines", "seedlings", "shseedlings")) {
      shiny::validate(shiny::need(FALSE,base::paste("Basal area / percent cover is not available for", input$densGroup,
                                                    "- try 'Abundance' or 'Proportion of Plots Occupied'.")))}
    
    n_plots <- base::tryCatch(base::nrow(NPSForVeg::getPlots(veg, years = densYears(), type = "all")), error = function(e) NA_integer_)
    
    if (!base::is.na(n_plots) && n_plots < 2) {
      sxs <- base::tryCatch({
        args <- base::list(
          object = veg,
          group  = input$densGroup,
          years  = densYears(),
          values = input$densvalues,
          area   = if (input$densvalues == "presab") "plot" else "ha")
        if (input$densGroup %in% base::c("trees", "saplings")) {
          args$status <- "alive"}
        base::do.call(NPSForVeg::SiteXSpec, args)
      }, error = function(e) {message("SiteXSpec fallback error for ", input$densGroup, ":", conditionMessage(e)) 
        NULL})
      
      species_cols <- base::setdiff(base::names(sxs), base::c("Plot_Name", "Total"))
      
      result <- if (base::is.null(sxs) || base::nrow(sxs) == 0 || base::length(species_cols) == 0) {
        NULL
      } else {
        base::data.frame(
          Latin_Name = species_cols,
          Mean       = base::as.numeric(sxs[1, species_cols]),
          Lower.95   = NA_real_,
          Upper.95   = NA_real_,
          stringsAsFactors = FALSE)}
      
      base::return(result)}
    
    result <- base::tryCatch({
      base::set.seed(42)
      NPSForVeg::dens(
        object = veg,
        group  = input$densGroup,
        years  = densYears(),
        values = input$densvalues,
        common = FALSE,
        area   = if (input$densvalues == "presab") "plot" else "ha",
        Total  = FALSE)},
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("size", msg, ignore.case = TRUE)) {
          shiny::validate(shiny::need(FALSE, base::paste("No size measurement available for this plant group:", input$densGroup)))}
        message("densData error (likely sparse data): ", msg) 
        NULL})
    
    if (!base::is.null(result) && base::nrow(result) == 0) result <- NULL
    
    result
  }) ###################################
  
  #### common names checkbox ####
  species_col <- shiny::reactive(if (base::isTRUE(input$densCommon)) "Common_Name" else "Latin_Name")
  
  ### summary statistics checkbox ###
  text_on <- shiny::reactive(base::isTRUE(input$plotlyText))
  
  #### create base plotting df #####
  densDf <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    raw <- densData()
    
    shiny::validate(shiny::need(
      !base::is.null(raw) && base::nrow(raw) > 0,
      base::paste("No", base::switch(input$densGroup, trees = "tree", saplings = "sapling",
                                     seedlings = "tree seedling", shrubs = "shrub", shseedlings = "shrub seedling",
                                     herbs = "understory plant", vines = "vine", input$densGroup),
                  "observations were recorded at",
                  NPSForVeg::getNames(VEGDATA[[input$densPark]], "long"),
                  "during", base::min(densYears()), "-", base::max(densYears()), ".")))
    
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
    
    ### radioButton: Least ###
    if (base::identical(input$densSpeciesType, "Least")) {
      shiny::req(input$densTop)
      df <- df %>% dplyr::arrange(Mean) %>% dplyr::slice(1:input$densTop)}
    
    ### radioButton: All ###
    if (base::identical(input$densSpeciesType, "All")) {
      agg_fun <- if (input$densvalues %in% base::c("count","size")) sum else mean
      df <- df %>%
        dplyr::summarise(
          Species = "All species",
          Latin = "All species",
          Common = "All species",
          Mean = agg_fun(Mean, na.rm = TRUE),
          err_up = agg_fun(err_up, na.rm = TRUE),
          err_dn = agg_fun(err_dn, na.rm = TRUE)) %>%
        dplyr::mutate(LabelOpp = "All species")
    } else {
      df <- df %>% dplyr::mutate(
        LabelOpp = dplyr::case_when(
          base::isTRUE(input$densCommon) ~ Latin,
          !base::isTRUE(input$densCommon) ~ dplyr::coalesce(Common, Latin)))}
    
    df <- df %>%
      dplyr::mutate(.tie = base::seq_along(Species)) %>%
      dplyr::arrange(dplyr::desc(Mean), .tie) %>%
      dplyr::mutate(Species = base::factor(Species, levels = Species)) %>%
      dplyr::select(-.tie)
  })
  
  #### create compare plotting df #####
  
  ###bugfix update###########################
  dens_all_parks <- function(VEGDATA, group, years, values, common = FALSE) {
    area_val <- if (values == "presab") "plot" else "ha"
    
    per_park <- base::lapply(base::names(VEGDATA), function(pk) {
      x <- VEGDATA[[pk]]
      out <- base::tryCatch({
        base::set.seed(42)
        NPSForVeg::dens(object = x, group = group, years = years,
                        values = values, common = common,
                        area = area_val, Total = FALSE)
      },
      error = function(e) {
        message("dens_all_parks: skipping park '", pk, "': ", conditionMessage(e))
        NULL
      })
      if (base::is.null(out) || base::nrow(out) == 0) base::return(NULL)
      out
    })
    
    per_park <- base::Filter(base::Negate(base::is.null), per_park)
    if (!base::length(per_park)) base::return(NULL)
    
    combined_raw <- dplyr::bind_rows(per_park) %>%
      dplyr::filter(!base::is.na(Mean), Mean > 0)  # only where species actually occurs
    
    if (base::nrow(combined_raw) == 0) base::return(NULL)
    
    species_list <- base::unique(combined_raw$Latin_Name)
    
    result_rows <- base::lapply(species_list, function(sp) {
      d <- combined_raw[combined_raw$Latin_Name == sp, ]
      if (base::nrow(d) == 1) {
        base::return(base::data.frame(
          Latin_Name = sp, Mean = d$Mean[1],
          Lower.95 = d$Lower.95[1], Upper.95 = d$Upper.95[1],
          stringsAsFactors = FALSE))}
      SE <- (d$Upper.95 - d$Lower.95) / (2 * 1.96)
      w <- base::ifelse(!base::is.na(SE) & SE > 0, 1 / SE^2, NA_real_)
      
      if (base::all(base::is.na(w))) {
        base::data.frame(Latin_Name = sp, Mean = base::mean(d$Mean, na.rm = TRUE),
                         Lower.95 = NA_real_, Upper.95 = NA_real_, stringsAsFactors = FALSE)
      } else {
        m <- stats::weighted.mean(d$Mean, w, na.rm = TRUE)
        se_comb <- base::sqrt(1 / base::sum(w, na.rm = TRUE))
        base::data.frame(Latin_Name = sp, Mean = m, Lower.95 = m - 1.96 * se_comb, Upper.95 = m + 1.96 * se_comb, stringsAsFactors = FALSE)}})

    dplyr::bind_rows(result_rows)
  }##########################################
  
  compareDf <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    cmp <- DensCompare()
    if (base::is.null(cmp) || (base::is.atomic(cmp) && base::is.na(cmp))) base::return(NULL)
    shiny::req(base::is.list(cmp), cmp$group, cmp$years, cmp$values)

    if (base::identical(cmp$park, "ALL")) {
      raw <- dens_all_parks(
        VEGDATA = VEGDATA,
        group = cmp$group,
        years = cmp$years,
        values = cmp$values)
      
      if (base::is.null(raw) || base::nrow(raw) == 0) base::return(NULL)
      
      # Align to base species — fill missing with zeros
      base_species <- densData()$Latin_Name
      missing_species <- base::setdiff(base_species, raw$Latin_Name)
      
      if (base::length(missing_species) > 0) {
        empty_rows <- base::data.frame(
          Latin_Name = missing_species,
          Mean = 0,
          Lower.95 = 0,
          Upper.95 = 0,
          stringsAsFactors = FALSE)
        extra_cols <- base::setdiff(names(raw), base::names(empty_rows))
        for (col in extra_cols) empty_rows[[col]] <- NA
        raw <- dplyr::bind_rows(raw, empty_rows[, base::names(raw), drop = FALSE])}
      
      raw <- raw %>% dplyr::select(-dplyr::any_of("Park"))
      
    } else {

      shiny::req(cmp$object)
      cmp_nplots <- base::tryCatch(
        base::nrow(NPSForVeg::getPlots(cmp$object, years = cmp$years, type = "all")),
        error = function(e) NA_integer_)
      
      if (!base::is.na(cmp_nplots) && cmp_nplots < 2) {
        cmp_args <- base::list(
          object = cmp$object,
          group  = cmp$group,
          years  = cmp$years,
          values = cmp$values,
          area   = cmp$area)
        if (cmp$group %in% base::c("trees", "saplings")) cmp_args$status <- "alive"
        sxs <- base::tryCatch(base::do.call(NPSForVeg::SiteXSpec, cmp_args), error = function(e) NULL)
        species_cols <- base::setdiff(base::names(sxs), base::c("Plot_Name", "Total"))
        raw <- if (base::is.null(sxs) || base::nrow(sxs) == 0 || base::length(species_cols) == 0) {
          NULL
        } else {
          base::data.frame(
            Latin_Name = species_cols,
            Mean       = base::as.numeric(sxs[1, species_cols]),
            Lower.95   = NA_real_,
            Upper.95   = NA_real_,
            stringsAsFactors = FALSE)}
      } else {
        base::set.seed(42)
        raw <- NPSForVeg::dens(
          object = cmp$object,
          group  = cmp$group,
          years  = cmp$years,
          values = cmp$values,
          common = FALSE,
          area   = cmp$area,
          Total  = FALSE)}
      
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
        Mean    = .data$Mean,
        err_up  = dplyr::case_when(
          cmp$values == "presab" & .data$Mean >= 1 ~ 0,
          TRUE ~ .data$Upper.95 - .data$Mean),
        err_dn  = dplyr::case_when(
          cmp$values == "presab" & .data$Mean <= 0 ~ 0,
          TRUE ~ .data$Mean - .data$Lower.95)
      ) %>%
      dplyr::filter(!base::tolower(Species) %in% base::c("total", "all species"))
    
    ###selection rules
    # Pick
    if (base::identical(input$densSpeciesType, "Pick")) {shiny::req(input$densSpecies)
      df <- df %>% dplyr::filter(Latin %in% input$densSpecies)}
    
    # Common = top N from BASE — align by Latin not display name
    if (base::identical(input$densSpeciesType, "Common") || base::identical(input$densSpeciesType, "Least")) {      base_latin <- if ("Latin" %in% base::names(densDf())) {base::as.character(densDf()$Latin)
      } else {base::as.character(densDf()$Species)}
      df <- df %>% dplyr::filter(Latin %in% base_latin)}
    
    # All species combined
    if (base::identical(input$densSpeciesType, "All")) {
      agg_fun <- if (cmp$values %in% base::c("count","size")) sum else mean
      df <- df %>%
        dplyr::summarise(
          Species = "All species",
          Latin = "All species",
          Common = "All species",
          Mean = agg_fun(Mean,   na.rm = TRUE),
          err_up = agg_fun(err_up, na.rm = TRUE),
          err_dn = agg_fun(err_dn, na.rm = TRUE)) %>%
        dplyr::mutate(LabelOpp = "All species")
      # return early — no zero-fill or factor alignment needed for All
      base::return(df)}
    
    df <- df %>% dplyr::mutate(
      LabelOpp = dplyr::case_when(
        base::isTRUE(input$densCommon) ~ Latin,
        TRUE ~ dplyr::coalesce(Common, Latin)))
    
    # zero-fill base species missing from compare — use Latin for matching
    base_latin <- if ("Latin" %in% base::names(densDf())) {base::as.character(densDf()$Latin)
    } else {base::as.character(densDf()$Species)}
    missing_latin <- base::setdiff(base_latin, df$Latin)
    if (base::length(missing_latin) > 0) {
      base_df_ref <- densDf()
      zero_rows <- base::data.frame(
        Species = base::as.character(base_df_ref$Species[base_df_ref$Latin %in% missing_latin]),
        Latin = missing_latin,
        Common = NA_character_,
        Mean = 0,
        err_up = NA_real_,
        err_dn = NA_real_,
        LabelOpp = base::as.character(base_df_ref$LabelOpp[base_df_ref$Latin %in% missing_latin]),
        stringsAsFactors = FALSE)
      df <- dplyr::bind_rows(df, zero_rows)}
    
    df$Species <- base::factor(df$Species, levels = base::union(
      base::levels(densDf()$Species),
      base::as.character(df$Species)))
    
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
  park_long_name <- function(park_key) {
    if (base::is.null(park_key) || !base::nzchar(park_key)) base::return(park_key)
    obj <- VEGDATA[[park_key]]
    if (!base::is.null(obj)) {NPSForVeg::getNames(obj, "long")
    } else {park_key}}
  
  densMissingWarningData <- shiny::reactive({
    base_df <- base::tryCatch(densDf(), error = function(e) NULL)
    cmp_df <- base::tryCatch(compareDf(), error = function(e) NULL)
    
    if (base::is.null(base_df) || base::nrow(base_df) == 0) base::return(NULL)
    if (base::is.null(cmp_df) || base::nrow(cmp_df) == 0) base::return(NULL)
    
    merged <- dplyr::left_join(
      base_df %>% dplyr::select(Latin, Species),
      cmp_df  %>% dplyr::select(Latin, Mean),
      by = "Latin")
    
    missing <- merged %>%
      dplyr::filter(base::is.na(Mean) | Mean == 0)
    
    if (base::nrow(missing) == 0) base::return(NULL)
    
    base::list(n = base::nrow(missing), base_name = park_long_name(input$densPark), cmp_name = if (input$CompareType == "Park") {
        if (base::identical(input$ComparePark, "ALL")) {"All Parks"
        } else {park_long_name(input$ComparePark)}
      } else if (input$CompareType == "Time") {"comparison period"
      } else if (input$CompareType == "Growth Stage") {"comparison growth stage"
      } else {"comparison dataset"})})
  
  densWarningDismissed <- shiny::reactiveVal(FALSE)
  
  shiny::observeEvent(
    base::list(input$densPark, input$densGroup, input$densCycles, input$densvalues, input$CompareType, input$ComparePark, input$compCycles, input$CompareGroup),
    {densWarningDismissed(FALSE)})
  
  shiny::observeEvent(input$dismiss_dens_warning,
    {densWarningDismissed(TRUE)})
  
  output$densGraphMessage <- shiny::renderUI({
    if (base::identical(input$densSpeciesType, "Pick") &&
        (base::is.null(input$densSpecies) || base::length(input$densSpecies) == 0)) {
      shiny::helpText("Please select one or more species.")
    } else { NULL }
  })
  
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
                                          base::paste0("Warning: ", n, " ", base_group, " species ", verb, " not present as ", cmp_stage, " in ", info$base_name, " (shown as 0 on the figure).")},
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
  
  # summary report
  densReportText <- shiny::reactive({
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    df <- base::tryCatch(densDf(), error = function(e) NULL)
    if (base::is.null(df) || base::nrow(df) == 0) base::return(NULL)
    
    park_label <- NPSForVeg::getNames(VEGDATA[[input$densPark]], "long")
    group_label <- base::switch(input$densGroup,
                                trees = "trees", saplings = "saplings", seedlings = "tree seedlings",
                                shrubs = "shrubs", shseedlings = "shrub seedlings",
                                herbs = "understory plants", vines = "vines", input$densGroup)
    val_label <- base::switch(input$densvalues,
                              count = base::switch(input$densGroup,
                                                   trees = "trees per hectare", saplings = "saplings per hectare",
                                                   seedlings = "tree seedlings per hectare", shrubs = "shrubs per hectare",
                                                   shseedlings = "shrub seedlings per hectare", vines = "vines per hectare",
                                                   "individuals per hectare"),
                              size = base::switch(input$densGroup,
                                                  trees =, saplings = "square meters of basal area per hectare",
                                                  herbs = "percent cover", cwd = "cubic meters per hectare",
                                                  "size units per hectare"),
                              presab = "proportion of plots occupied")
    period <- base::paste0(base::min(densYears()), "\u2013", base::max(densYears()))
    
    status_phrase <- if (input$densGroup == "trees") {
      base::switch(input$TreeStatus, alive = "living trees", snag = "snags (dead standing trees)", all = "all trees (living and dead)", "trees")
    } else { group_label }
    
    n_sp <- base::nrow(df)
    
    stage_order <- base::c("seedlings", "saplings", "trees", "shseedlings", "shrubs")
    stage_labels <- base::c(trees = "Trees", saplings = "Saplings", seedlings = "Tree seedlings", shrubs = "Shrubs", shseedlings = "Shrub seedlings")
    
    # compare label intro
    cmp_label_intro <- if (!base::identical(input$CompareType, "None")) {
      base::switch(input$CompareType,
                   Park = {if (base::identical(input$ComparePark, "ALL")) "All Parks"
                     else {cmp_obj <- VEGDATA[[input$ComparePark]]
                     if (!base::is.null(cmp_obj)) NPSForVeg::getNames(cmp_obj, "long") else input$ComparePark}},
                   "Growth Stage" = stage_labels[[input$CompareGroup]],
                   Time = {row <- DATACYCLES[DATACYCLES$Cycle == input$compCycles, ]
                   if (base::nrow(row) == 1L) base::paste0("Cycle ", row$Cycle, ": ", row$YearStart, "\u2013", row$YearEnd)
                   else base::as.character(input$compCycles)},
                   "Comparison")
    } else { NULL }
    
    base_cycle_label <- {row <- DATACYCLES[DATACYCLES$Cycle == input$densCycles, ]
    if (base::nrow(row) == 1L) base::paste0("Cycle ", row$Cycle, ": ", row$YearStart, "\u2013", row$YearEnd)
    else period}
    
    # intro sentence
    intro <- if (base::identical(input$CompareType, "None")) {base::sprintf("Showing %d species of %s recorded at %s from %s.", n_sp, status_phrase, park_label, period)
    } else {base::switch(input$CompareType,
                   Park = base::sprintf("Comparing %d species of %s at %s vs. %s (%s).", n_sp, status_phrase, park_label, cmp_label_intro, period),
                   "Growth Stage" = {
                     base_idx <- base::match(input$densGroup, stage_order)
                     cmp_idx <- base::match(input$CompareGroup, stage_order)
                     base_stage <- stage_labels[[input$densGroup]]
                     cmp_stage <- stage_labels[[input$CompareGroup]]
                     if (base_idx <= cmp_idx) {base::sprintf("Comparing %d species: %s vs. %s at %s (%s).", n_sp, base_stage, cmp_stage, park_label, period)
                     } else {base::sprintf("Comparing %d species: %s vs. %s at %s (%s).", n_sp, cmp_stage, base_stage, park_label, period)}},
                   Time = {
                     base_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$densCycles]
                     cmp_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$compCycles]
                     if (base_yr <= cmp_yr) {base::sprintf("Comparing %d species of %s at %s: %s vs. %s.", n_sp, status_phrase, park_label, base_cycle_label, cmp_label_intro)
                     } else {base::sprintf("Comparing %d species of %s at %s: %s vs. %s.", n_sp, status_phrase, park_label, cmp_label_intro, base_cycle_label)}},
                   base::sprintf("Showing %d species of %s recorded at %s from %s.", n_sp, status_phrase, park_label, period))}
    
    # bullet points
    bullet_tags <- base::lapply(base::seq_len(n_sp), function(i) {
      sp <- base::as.character(df$Species[i])
      opp <- if (!base::is.null(df$LabelOpp) && !base::is.na(df$LabelOpp[i]) && base::nzchar(df$LabelOpp[i])) {base::paste0(" (", df$LabelOpp[i], ")")
      } else { "" }
      mean <- base::round(df$Mean[i], 2)
      low <- if (!base::is.na(df$err_dn[i])) base::round(df$Mean[i] - df$err_dn[i], 2) else NA
      high <- if (!base::is.na(df$err_up[i])) base::round(df$Mean[i] + df$err_up[i], 2) else NA
      ci_str <- if (!base::is.na(low) && !base::is.na(high)) {base::sprintf(" | 95%% CI: %.2f\u2013%.2f", low, high)
      } else { "" }
      htmltools::tags$li(htmltools::tags$strong(sp), opp, base::sprintf(" \u2014 Mean: %.2f %s%s.", mean, val_label, ci_str))})
    
    # sentence, no compare
    max_row <- df[base::which.max(df$Mean), ]
    min_row <- df[base::which.min(df$Mean), ]
    top_sentence <- if (n_sp > 1 && base::identical(input$CompareType, "None")) {
      base::sprintf("Among the %d species shown, %s had the highest mean %s (%.2f), while %s had the lowest (%.2f).",
                    n_sp, base::as.character(max_row$Species[1]), val_label, base::round(max_row$Mean[1], 2), base::as.character(min_row$Species[1]), base::round(min_row$Mean[1], 2))
    } else { NULL }
    
    # compare section
    compare_section <- if (!base::identical(input$CompareType, "None")) {
      df_cmp <- base::tryCatch(compareDf(), error = function(e) NULL)
      
      if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) {
        
        cmp_label <- base::switch(input$CompareType,
                                  Park = {if (base::identical(input$ComparePark, "ALL")) "All Parks"
                                    else {cmp_obj <- VEGDATA[[input$ComparePark]]
                                    if (!base::is.null(cmp_obj)) NPSForVeg::getNames(cmp_obj, "long") else input$ComparePark}},
                                  "Growth Stage" = {lbl <- stage_labels[[input$CompareGroup]]
                                    if (base::identical(input$densGroup, input$CompareGroup)) {base::paste0(lbl)
                                    } else { lbl }},
                                  Time = {row <- DATACYCLES[DATACYCLES$Cycle == input$compCycles, ]
                                  if (base::nrow(row) == 1L) base::paste0("Cycle ", row$Cycle, ": ", row$YearStart, "\u2013", row$YearEnd)
                                  else base::as.character(input$compCycles)}, "Comparison")
        
        df_cmp_clean <- if (base::identical(input$densSpeciesType, "All")) {df_cmp
        } else {df_cmp %>%
            dplyr::filter(!base::tolower(base::as.character(Species)) %in% base::c("total", "all species"))}
        
        paired_bullets <- base::lapply(base::seq_len(n_sp), function(i) {
          sp <- base::as.character(df$Species[i])
          opp <- if (!base::is.null(df$LabelOpp) && !base::is.na(df$LabelOpp[i]) && base::nzchar(df$LabelOpp[i])) {
            base::paste0(" (", df$LabelOpp[i], ")")
          } else { "" }
          
          # base values
          base_mean <- base::round(df$Mean[i], 2)
          base_low  <- if (!base::is.na(df$err_dn[i])) base::round(df$Mean[i] - df$err_dn[i], 2) else NA
          base_high <- if (!base::is.na(df$err_up[i])) base::round(df$Mean[i] + df$err_up[i], 2) else NA
          base_ci   <- if (!base::is.na(base_low) && !base::is.na(base_high)) {
            base::sprintf(" | 95%% CI: %.2f\u2013%.2f", base_low, base_high)
          } else { "" }
          
          # base row label
          base_row_label <- base::switch(input$CompareType,
                                         Park = park_label,
                                         "Growth Stage" = {lbl <- stage_labels[[input$densGroup]]
                                           if (base::identical(input$densGroup, input$CompareGroup)) {base::paste0(lbl)
                                           } else { lbl }},
                                         Time = base_cycle_label,
                                         park_label)
          
          # get latin for stable matching
          base_latin <- if ("Latin" %in% base::names(df)) {base::as.character(df$Latin[i])
          } else { sp }
          
          # match compare row by Latin first, fall back to Species
          cmp_row <- if (base::identical(input$densSpeciesType, "All")) {df_cmp_clean
          } else {matched <- df_cmp_clean %>%
            dplyr::filter(if ("Latin" %in% base::names(df_cmp_clean)) {base::as.character(Latin) == base_latin
              } else {base::as.character(Species) == sp})
            if (base::nrow(matched) == 0) {df_cmp_clean %>% dplyr::filter(base::as.character(Species) == sp)
            } else { matched }}
          
          cmp_str <- if (base::nrow(cmp_row) > 0 && !base::is.na(cmp_row$Mean[1]) && cmp_row$Mean[1] > 0) {
            cmp_mean <- base::round(cmp_row$Mean[1], 2)
            cmp_low  <- if (!base::is.na(cmp_row$err_dn[1])) base::round(cmp_row$Mean[1] - cmp_row$err_dn[1], 2) else NA
            cmp_high <- if (!base::is.na(cmp_row$err_up[1])) base::round(cmp_row$Mean[1] + cmp_row$err_up[1], 2) else NA
            cmp_ci <- if (!base::is.na(cmp_low) && !base::is.na(cmp_high)) {
              base::sprintf(" | 95%% CI: %.2f\u2013%.2f", cmp_low, cmp_high)
            } else { "" }
            base::sprintf("%s: %.2f %s%s", cmp_label, cmp_mean, val_label, cmp_ci)
          } else { base::sprintf("%s: not observed", cmp_label) }
          
          # order bullets
          base_li <- htmltools::tags$li(base::sprintf("%s: %.2f %s%s", base_row_label, base_mean, val_label, base_ci))
          cmp_li <- htmltools::tags$li(cmp_str)
          
          sub_bullets <- if (base::identical(input$CompareType, "Time")) {
            base_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$densCycles]
            cmp_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$compCycles]
            if (base_yr <= cmp_yr) base::list(base_li, cmp_li) else base::list(cmp_li, base_li)
          } else if (base::identical(input$CompareType, "Growth Stage")) {
            base_idx <- base::match(input$densGroup, stage_order)
            cmp_idx <- base::match(input$CompareGroup, stage_order)
            if (base_idx <= cmp_idx) base::list(base_li, cmp_li) else base::list(cmp_li, base_li)
          } else { base::list(base_li, cmp_li) }
          
          htmltools::tags$li(htmltools::tags$strong(sp), opp, htmltools::tags$ul(style = "list-style: none; margin: 2px 0 2px 16px; padding: 0;", sub_bullets))})
        
        htmltools::tagList(paired_bullets)
        
      } else { NULL }
    } else { NULL }

    htmltools::tagList(
      htmltools::tags$p(htmltools::tags$span(style = "font-size: 30px; font-weight: bold;", "Summary Report:")),
      htmltools::tags$p(style = "font-size: 15px;", htmltools::tags$strong(style = "font-size: 20px;", park_label), " \u2014 ", intro),
      if (base::identical(input$CompareType, "None")) {htmltools::tags$ul(style = "font-size: 15px; margin: 6px 0 6px 16px; padding: 0;", bullet_tags)
      } else {htmltools::tags$ul(style = "font-size: 15px; margin: 6px 0 6px 16px; padding: 0;", compare_section)},
      if (!base::is.null(top_sentence)) htmltools::tags$p(style = "font-size: 15px;", top_sentence) else NULL,
      htmltools::tags$p(style = "font-size: 15px;",htmltools::tags$em("Species averages with 95% confidence intervals.")))})
  
  output$densReportGraph <- shiny::renderUI({
    txt <- base::tryCatch(densReportText(), error = function(e) NULL)
    if (base::is.null(txt)) base::return(NULL)
    htmltools::tagList(
      htmltools::tags$div(
        style = "margin-bottom: 12px;",
        htmltools::tags$div(
          class = "report-header",
          style = "cursor: pointer; font-size: 13px; font-weight: bold; padding: 8px 12px;
               background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px;
               user-select: none; display: flex; align-items: center; gap: 6px;",
          htmltools::tags$span(class = "report-caret", "\u25bc"),
          "Summary Report"
        ),
        htmltools::tags$div(
          class = "report-body",
          style = "padding: 0 16px; background-color: #f8f9fa;
               border: 1px solid #dee2e6; border-top: none;
               border-radius: 0 0 6px 6px; font-size: 13px; line-height: 1.8;",
          txt)))})
  
  output$densReportTable <- shiny::renderUI({
    txt <- base::tryCatch(densReportText(), error = function(e) NULL)
    if (base::is.null(txt)) base::return(NULL)
    htmltools::tagList(
      htmltools::tags$div(
        style = "margin-bottom: 12px;",
        htmltools::tags$div(
          class = "report-header",
          style = "cursor: pointer; font-size: 13px; font-weight: bold; padding: 8px 12px;
               background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px;
               user-select: none; display: flex; align-items: center; gap: 6px;",
          htmltools::tags$span(class = "report-caret", "\u25bc"),
          "Summary Report"
        ),
        htmltools::tags$div(
          class = "report-body",
          style = "padding: 0 16px; background-color: #f8f9fa;
               border: 1px solid #dee2e6; border-top: none;
               border-radius: 0 0 6px 6px; font-size: 13px; line-height: 1.8;",
          txt)))})
  
  
  output$DensPlotly <- plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(
        !base::is.null(input$densPark) &&
          base::nzchar(input$densPark) &&
          !(base::identical(input$densSpeciesType, "Pick") &&
              (base::is.null(input$densSpecies) ||
                 base::length(input$densSpecies) == 0)),
        "There are no results for this combination of choices. Please select a park, species, or plant type."
      )
    )
    
    shiny::req(input$densGroup, input$densvalues, densYears())
    
    
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
    
    species_levels <- if (!base::is.null(df) && base::nrow(df) > 0) {
      base::as.character(df$Species)} else {
        base::unique(base::c(df$Species, if (!base::is.null(df_cmp)) df_cmp$Species))}
    species_levels <- base::unique(species_levels)
    
    if (!base::is.null(df) && base::nrow(df) > 0) {
      df$Species <- base::factor(df$Species, levels = species_levels)}
    if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) {
      df_cmp$Species <- base::factor(df_cmp$Species, levels = species_levels)}
    
    #make labels readable
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
    
    park_display <- park_long_name(input$densPark)
    cmp_park_display <- park_long_name(input$ComparePark)
    cycle_display_base <- cycle_long_name(input$densCycles)
    cycle_display_cmp <- cycle_long_name(input$compCycles)
    group_display_base <- group_long_name(input$densGroup)
    group_display_cmp <- group_long_name(input$CompareGroup)
    
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
    
    plotCfg <- buildPlotlyTitleLegend(
      title_text = DensTitle(),
      font_size = if (!base::is.null(input$densFontSize)) input$densFontSize else 12,
      screen_width = if (!base::is.null(input$densPlotContainer_width)) input$densPlotContainer_width else input$screenW,
      legend_labels = if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) base::c(base_legend, cmp_legend) else base::c(base_legend))
    
    base_legend <- plotCfg$legend_labels[[1]]
    if (!base::is.null(df_cmp) && base::nrow(df_cmp) > 0) cmp_legend <- plotCfg$legend_labels[[2]]
    
    # graphing colors
    densBaseColor <- toHex(pickColor(input$densBaseColor, "blue"))
    densCmpColor <- toHex(pickColor(input$densCompareColor, "red"))
    densFontSize <- if (!base::is.null(input$densFontSize)) input$densFontSize else 12
    
    # error bar colors
    darken <- function(hex, factor = 0.6) {rgb <- grDevices::col2rgb(hex)
    grDevices::rgb(rgb[1] * factor, rgb[2] * factor, rgb[3] * factor, maxColorValue = 255)}
    
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
      bar_color <- if (grp == base_legend) densBaseColor else densCmpColor
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
      hoverlabel = base::list(
        bgcolor = "white",
        bordercolor = bar_color,
        font = base::list(color = "black")),
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
      title = plotCfg$title,
      legend = plotCfg$legend,
      xaxis = base::list(
        showgrid = TRUE,
        title = base::list(text = densYlabel(), font = base::list(size = densFontSize), standoff = 20)),
      yaxis = base::list(
        type = "category", categoryorder = "array", categoryarray = rev(species_levels),
        showline = TRUE,
        title = base::list(text = "Species", font = base::list(size = densFontSize), standoff = 15),
        ticks = "outside", ticklabelposition = "outside"),
      margin = plotCfg$margin,
      height = plotCfg$height,
      font = base::list(size = densFontSize),
      autosize = TRUE)
    
    p <- p %>% plotly::config(
      displayModeBar = base::is.null(input$screenW) || input$screenW >= 768,
      displaylogo = FALSE)
    p
  })
  
  ########## make dens plotly warning reactive correctly like iv example above ^^^, also check if it applies to compare plot too
  
  # footnote
  output$DensLimitWarning <- shiny::renderUI({
    shiny::req(
      !base::is.null(input$densPark) && base::nzchar(input$densPark),
      !(base::identical(input$densSpeciesType, "Pick") &&
          (base::is.null(input$densSpecies) || base::length(input$densSpecies) == 0)))
    df <- base::tryCatch(densDf(), error = function(e) NULL)
    shiny::req(!base::is.null(df), base::nrow(df) > 0)
    if (!base::identical(input$CompareType, "None")) {
      df_cmp <- base::tryCatch(compareDf(), error = function(e) NULL)
      shiny::req(!base::is.null(df_cmp), base::nrow(df_cmp) > 0)
    }
    htmltools::tags$div(
      style = "font-size: 12px; color: #888; font-style: italic; margin-top: 6px; text-align: center;",
      "* Note: Not all species names may be shown on the plot above. See the summary report or data table for the full list.")})
  
  ####### original graphs and file downloads #######
  #tempDensPlot<-shiny::reactive({
  #  if (base::is.null(input$densPark) || base::nchar(input$densPark)==0) {base::return()}
  #    else{
  #      shiny::validate(shiny::need(base::try(
  #        base::do.call(densplot,DensPlotArgs() )),
  #       "There are no results for this combination of choices. The type of plant you selected was not found in the park during those years."
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
                                "There are no results for this combination of choices. Please select a park, species, or plant type."))
    shiny::req(input$densPark, input$densGroup, input$densvalues, densYears())
    raw <- densData()
    shiny::validate(shiny::need(
      !base::is.null(raw) && base::nrow(raw) > 0,
      base::paste("No", base::switch(input$densGroup, trees = "tree", saplings = "sapling", seedlings = "tree seedling", shrubs = "shrub", shseedlings = "shrub seedling", 
                                     herbs = "understory plant", vines = "vine", input$densGroup), "observations were recorded at", 
                  NPSForVeg::getNames(VEGDATA[[input$densPark]], "long"), "during", base::min(densYears()), "-", base::max(densYears()), ".")))
    
    if (!("Common_Name" %in% base::names(raw))) {
      raw$Common_Name <- base::tryCatch(
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
      if (base::identical(input$densSpeciesType, "Least")) {
        shiny::req(input$densTop)
        tbl <- tbl %>% dplyr::arrange(Mean) %>% dplyr::slice(1:input$densTop)}
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
          ~ base::ifelse(base::is.na(.x), "NA", base::format(base::round(base::as.numeric(.x), 2), nsmall = 2, scientific = FALSE))))}
    
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
        Species  = base::as.character(Species),
        Mean     = base::as.numeric(Mean),
        Lower95  = base::as.numeric(Mean) - base::as.numeric(err_dn),
        Upper95  = base::as.numeric(Mean) + base::as.numeric(err_up))
    
    # for All species — do NOT filter out "All species" row
    if (!base::identical(input$densSpeciesType, "All")) {
      cmp_half <- cmp_half %>%
        dplyr::filter(!base::tolower(Species) %in% base::c("total"))}
    
    # fill 0 if species is absent in compare (not needed for All)
    if (!base::identical(input$densSpeciesType, "All")) {
      base_species <- base::as.character(base_half$Species)
      missing <- base::setdiff(base_species, base::as.character(cmp_half$Species))
      if (base::length(missing) > 0) {
        zero_rows <- base::data.frame(
          Species  = missing,
          Mean     = 0,
          Lower95  = NA_real_,
          Upper95  = NA_real_,
          stringsAsFactors = FALSE)
        cmp_half <- dplyr::bind_rows(cmp_half, zero_rows)}}
    
    # reactive labels
    base_obj   <- VEGDATA[[input$densPark]]
    base_label <- base::switch(input$CompareType,
                               Park         = NPSForVeg::getNames(base_obj, "long"),
                               "Growth Stage" = DENSLABELDATA$Label[DENSLABELDATA$Name == input$densGroup],
                               Time         = {row <- DATACYCLES[DATACYCLES$Cycle == input$densCycles, ]
                               if (base::nrow(row) == 1L) base::paste0(row$Name, ":", row$YearStart, "-", row$YearEnd)
                               else base::as.character(input$densCycles)},
                               "Base")
    
    cmp_label <- base::switch(input$CompareType,
                              Park         = {if (base::identical(input$ComparePark, "ALL")) "All Parks"
                                else {cmp_obj <- VEGDATA[[input$ComparePark]]
                                if (!base::is.null(cmp_obj)) NPSForVeg::getNames(cmp_obj, "long") else input$ComparePark}},
                              "Growth Stage" = DENSLABELDATA$Label[DENSLABELDATA$Name == input$CompareGroup],
                              Time         = {row <- DATACYCLES[DATACYCLES$Cycle == input$compCycles, ]
                              if (base::nrow(row) == 1L) base::paste0(row$Name, ":", row$YearStart, "-", row$YearEnd)
                              else base::as.character(input$compCycles)},
                              "Compare")
    
    base_on_top <- base::switch(input$CompareType,
                                Park           = TRUE,
                                "Growth Stage" = {
                                  stage_order <- base::c("seedlings", "saplings", "trees", "shseedlings", "shrubs")
                                  base_idx <- base::match(input$densGroup,    stage_order)
                                  cmp_idx  <- base::match(input$CompareGroup, stage_order)
                                  base_idx <= cmp_idx},
                                Time = {
                                  base_yr <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$densCycles]
                                  cmp_yr  <- DATACYCLES$YearStart[DATACYCLES$Cycle == input$compCycles]
                                  base_yr <= cmp_yr},
                                TRUE)
    
    dataset_levels <- base::unique(
      if (base_on_top) base::c(base_label, cmp_label) else base::c(cmp_label, base_label))
    
    base_half$Dataset <- base_label
    cmp_half$Dataset  <- cmp_label
    
    # for All species — simple two-row bind, no factor alignment needed
    if (base::identical(input$densSpeciesType, "All")) {
      combined <- dplyr::bind_rows(
        base_half %>% dplyr::mutate(Dataset = base_label),
        cmp_half  %>% dplyr::mutate(Dataset = cmp_label)) %>%
        dplyr::arrange(base::factor(Dataset, levels = dataset_levels)) %>%
        dplyr::select(Species, Dataset, Mean, Lower95, Upper95)
      base::return(fmt(combined))}
    
    sp_levels <- base::unique(base_half$Species)
    
    combined <- dplyr::bind_rows(
      base_half %>%
        dplyr::mutate(
          Species = base::factor(base::as.character(Species), levels = sp_levels),
          Dataset = base_label),
      cmp_half %>%
        dplyr::mutate(
          Species = base::factor(base::as.character(Species), levels = sp_levels),
          Dataset = cmp_label)) %>%
      dplyr::arrange(Species, base::factor(Dataset, levels = dataset_levels)) %>%
      dplyr::group_by(Species) %>%
      dplyr::mutate(Species = dplyr::if_else(
        dplyr::row_number() == 1,
        base::as.character(Species),
        "")) %>%
      dplyr::ungroup() %>%
      dplyr::select(Species, Dataset, Mean, Lower95, Upper95)
    
    fmt(combined)})
  
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

  
  
  
  
  
  
  #### Time Series Panel ####

  # park control
  output$tsParkControl <- shiny::renderUI({
    shiny::selectizeInput(inputId = "tsPark", choices = base::c("All Parks" = "All", PARKLIST), label = "Park:",
                          options = base::list(
                            placeholder = 'Select a park',
                            onInitialize = base::I('function() { this.setValue(""); }')))})
  
  ts_all_parks <- function(VEGDATA, group, years, values) {dens_all_parks(VEGDATA = VEGDATA, group = group, years = years, values = values, common = FALSE)}
  
  # cycle control
  output$tsCycleControl <- shiny::renderUI({
    shiny::req(DATACYCLES, input$tsPark, input$tsGroup, input$tsValues)
    shiny::req(!base::is.null(input$tsPark), input$tsPark == "All" | input$tsPark %in% base::names(VEGDATA))
    
    cycle_vals <- base::sort(base::unique(DATACYCLES$Cycle))
    
    cycles_with_data <- base::Filter(function(cyc) {
      yrs <- DATACYCLES$YearStart[DATACYCLES$Cycle == cyc]:DATACYCLES$YearEnd[DATACYCLES$Cycle == cyc]
      base::tryCatch({
        if (input$tsPark == "All") {
          base::any(base::sapply(base::names(VEGDATA), function(pk) {
            plants <- NPSForVeg::getPlants(VEGDATA[[pk]], group = input$tsGroup, years = yrs, common = FALSE)
            !base::is.null(plants) && base::nrow(plants) > 0}))
        } else {plants <- NPSForVeg::getPlants(VEGDATA[[input$tsPark]], group = input$tsGroup, years = yrs, common = FALSE)
        !base::is.null(plants) && base::nrow(plants) > 0}}, error = function(e) FALSE)}, cycle_vals)
    
    shiny::req(base::length(cycles_with_data) >= 1)
    
    shiny::sliderInput(
      inputId = "tsCycles",
      label = "Display cycles:",
      min = base::min(cycles_with_data),
      max = base::max(cycles_with_data),
      value = base::c(base::min(cycles_with_data), base::max(cycles_with_data)),
      step = 1, ticks = FALSE)})

  # value control
  tsValuesUse <- shiny::reactive({
    base::switch(input$tsGroup,
                 trees =, saplings  = base::c(Abundance = "count", "Basal Area" = "size", "Proportion of Plots Occupied" = "presab"),
                 seedlings =, shseedlings =, shrubs =, vines = base::c(Abundance = "count", "Proportion of Plots Occupied" = "presab"),
                 herbs = base::c("Percent Cover" = "size", "Proportion of Plots Occupied" = "presab"),
                 cwd  = base::c("Volume" = "size"))})

  output$tsValControl <- shiny::renderUI({
    shiny::selectInput(inputId = "tsValues", label = "Data to Graph:", choices = tsValuesUse())})

  # axis label
  tsYlabel <- shiny::reactive({
    shiny::req(input$tsValues, input$tsGroup)
    base::switch(input$tsValues,
                 count = base::switch(input$tsGroup,
                                      trees = "Trees / ha",
                                      saplings = "Saplings / ha",
                                      seedlings = "Tree seedlings / ha",
                                      shrubs = "Shrubs / ha",
                                      shseedlings = "Shrub seedlings / ha",
                                      vines = "Vines on Trees / ha"),
                 size = base::switch(input$tsGroup,
                                     trees =, saplings = "Basal area (m\u00B2) / ha",
                                     herbs = "Percent Cover",
                                     cwd = "(m\u00B3) / ha"),
                 presab = "Proportion of Plots Occupied")})

  # species list
  tsSpecList <- shiny::reactive({
    shiny::req(input$tsPark, input$tsGroup, base::nzchar(input$tsPark))
    shiny::req(input$tsCycles, base::length(input$tsCycles) == 2)
    
    all_cycles <- base::sort(base::unique(DATACYCLES$Cycle))
    cycles_use <- all_cycles[all_cycles >= input$tsCycles[1] & all_cycles <= input$tsCycles[2]]
    shiny::req(base::length(cycles_use) > 0)
    
    name_obj <- if (input$tsPark == "All") VEGDATA[[PARKLIST[1]]] else VEGDATA[[input$tsPark]]
    
    spec_temp <- if (input$tsPark == "All") {
      all_yrs <- base::unique(base::unlist(base::lapply(cycles_use, function(cyc)
        DATACYCLES$YearStart[DATACYCLES$Cycle == cyc]:DATACYCLES$YearEnd[DATACYCLES$Cycle == cyc])))
      base::unique(base::unlist(base::lapply(names(VEGDATA), function(pk) {
        base::tryCatch(NPSForVeg::getPlants(VEGDATA[[pk]], group = input$tsGroup, years = all_yrs, common = FALSE)$Latin_Name, error = function(e) character(0))})))
    } else {all_yrs <- unlist(base::lapply(cycles_use, function(cyc)
      DATACYCLES$YearStart[DATACYCLES$Cycle == cyc]:DATACYCLES$YearEnd[DATACYCLES$Cycle == cyc]))
      base::tryCatch(NPSForVeg::getPlants(name_obj, group = input$tsGroup, years = all_yrs, common = FALSE)$Latin_Name, error = function(e) character(0))}
    spec_temp <- base::unique(spec_temp)
    
    shiny::req(base::length(spec_temp) > 0)
    
    spec_names <- fmt_common(base::tryCatch(
      NPSForVeg::getPlantNames(object = name_obj, names = spec_temp, in.style = "Latin", out.style = base::ifelse(input$tsCommon, "common", "Latin")),
      error = function(e) spec_temp))
    
    base::names(spec_temp) <- spec_names
    spec_temp[order(names(spec_temp))]
    }) %>% shiny::bindCache(input$tsPark, input$tsGroup, input$tsValues, input$tsCycles[1], input$tsCycles[2], input$tsCommon)
  
  tsSpeciesCount <- shiny::reactive({shiny::req(input$tsPark, input$tsGroup)
    base::length(tsSpecList())})

  output$tsSpeciesControl <- shiny::renderUI({
    input$tsCommon
    base::switch(input$tsSpeciesType,
                 Common = {
                   max_sp <- tsSpeciesCount()
                   htmltools::tags$div(
                     title = "Select the maximum number of species to plot",
                     shiny::sliderInput(
                       inputId = "tsTop",
                       label = "Maximum number of species to plot:",
                       min = 1,
                       max = base::max(1, max_sp, na.rm = TRUE),
                       value = base::min(5, max_sp),
                       step = 1,
                       ticks = FALSE))},
                 Least = {
                   max_sp <- tsSpeciesCount()
                   htmltools::tags$div(
                     title = "Select the maximum number of species to plot",
                     shiny::sliderInput(
                       inputId = "tsTop",
                       label = "Maximum number of species to plot (least common):",
                       min = 1,
                       max = base::max(1, max_sp, na.rm = TRUE),
                       value = base::min(5, max_sp),
                       step = 1,
                       ticks = FALSE))},
                 Pick = {if (base::is.null(input$tsPark) || base::length(input$tsPark) == 0) {base::return()
                 } else {
                   htmltools::tags$div(
                     title = "Click here to pick the species you want to graph",
                     shiny::selectizeInput(
                       inputId  = "tsSpecies",
                       label = "Select one or more species",
                       choices = tsSpecList(),
                       multiple = TRUE,
                       selected = input$tsSpecies,
                       options = base::list(
                         placeholder = "Select species to display",
                         plugins = base::list("remove_button"))))}},
                 All = NULL)})
  
  # ts reset buttons
  shiny::observeEvent(input$tsResetData, {
    shiny::updateSelectizeInput(session, "tsPark", selected = "")
    shiny::updateSelectizeInput(session, "tsGroup", selected = PLANTTYPES[[1]])
    shiny::updateCheckboxInput(session, "tsCommon", value = TRUE)
    shiny::updateCheckboxInput(session, "tsShowCI", value = FALSE)
    shiny::updateRadioButtons(session, "tsSpeciesType", selected = "Common")
    shiny::updateSelectizeInput(session, "tsSpecies", selected = base::character(0))
    shiny::updateRadioButtons(session, "tsTableOrder", selected = "species")
  })
  
  shiny::observeEvent(input$tsResetDisplay, {
    shiny::updateSliderInput(session, "tsLineThickness", value = 1.5)
    shiny::updateSliderInput(session, "tsFontSize", value = 12)
    shiny::updateSliderInput(session, "tsRibbonOpacity", value = 0.2)
    shiny::updateSelectizeInput(session, "tsColorPalette", selected = "set1")
  })
  
  tsDataIsDefault <- shiny::reactive({
    (base::is.null(input$tsPark)        || base::identical(input$tsPark, "")) &&
      (base::is.null(input$tsGroup)       || base::identical(input$tsGroup, PLANTTYPES[[1]])) &&
      (base::is.null(input$tsCommon)      || base::isTRUE(input$tsCommon)) &&
      (base::is.null(input$tsShowCI)      || !base::isTRUE(input$tsShowCI)) &&
      (base::is.null(input$tsSpeciesType) || base::identical(input$tsSpeciesType, "Common")) &&
      (base::is.null(input$tsSpecies)     || base::length(input$tsSpecies) == 0) &&
      (base::is.null(input$tsTableOrder)  || base::identical(input$tsTableOrder, "species"))
  })
  
  shiny::observe({
    shinyjs::toggleClass(id = "tsResetData", class = "btn-danger",
                         condition = !tsDataIsDefault())
  })
  
  tsDisplayIsDefault <- shiny::reactive({
    (base::is.null(input$tsLineThickness)  || input$tsLineThickness == 1.5) &&
      (base::is.null(input$tsFontSize)       || input$tsFontSize == 12) &&
      (base::is.null(input$tsRibbonOpacity)  || input$tsRibbonOpacity == 0.2) &&
      (base::is.null(input$tsColorPalette)   || base::identical(input$tsColorPalette, "set1"))
  })
  
  shiny::observe({
    shinyjs::toggleClass(id = "tsResetDisplay", class = "btn-danger",
                         condition = !tsDisplayIsDefault())
  })

  # keep species selection when toggling common/latin
  shiny::observeEvent(input$tsCommon, {
    shiny::req(input$tsPark, input$tsGroup, input$tsCycles)
    current <- shiny::isolate(input$tsSpecies)
    new_choices <- tsSpecList()
    shiny::req(!base::is.null(new_choices), base::length(new_choices) > 0)
    new_selected <- current[current %in% new_choices]
    shiny::updateSelectizeInput(session, "tsSpecies", choices = new_choices, selected = new_selected)})

  tsData <- shiny::reactive({
    shiny::req(input$tsPark, input$tsGroup, input$tsValues, input$tsCycles)
    shiny::req(base::nzchar(input$tsPark))
    
    all_cycles <- base::sort(base::unique(DATACYCLES$Cycle))
    cycles_use <- all_cycles[all_cycles >= input$tsCycles[1] & all_cycles <= input$tsCycles[2]]
    
    results <- base::lapply(cycles_use, function(cyc) {
      yr_start <- DATACYCLES$YearStart[DATACYCLES$Cycle == cyc]
      yr_end <- DATACYCLES$YearEnd[DATACYCLES$Cycle == cyc]
      yrs <- yr_start:yr_end

      df <- base::tryCatch(
        base::suppressWarnings({
          if (input$tsPark == "All") {
            ts_all_parks(
              VEGDATA = VEGDATA,
              group = input$tsGroup,
              years = yrs,
              values = input$tsValues)
          } else {
            base::set.seed(42)
            NPSForVeg::dens(
              object = VEGDATA[[input$tsPark]],
              group = input$tsGroup,
              years = yrs,
              values = input$tsValues,
              common = FALSE,
              area = if (input$tsValues == "presab") "plot" else "ha",
              Total = FALSE)}}),
        error = function(e) NULL)
      
      cycle_label <- base::paste0(DATACYCLES$Name[DATACYCLES$Cycle == cyc], " (", yr_start, "-", yr_end, ")")
      
      # return a placeholder row so the cycle still appears in the plot
      if (base::is.null(df) ||base::nrow(df) == 0) {
        base::return(base::data.frame(
          Latin_Name  = NA_character_,
          Mean = NA_real_,
          Lower.95 = NA_real_,
          Upper.95 = NA_real_,
          Cycle = cyc,
          CycleLabel = cycle_label,
          stringsAsFactors = FALSE))}
      
      df$Cycle <- cyc
      df$CycleLabel <- cycle_label
      df})
    
    results <- dplyr::bind_rows(results)
    
    shiny::validate(shiny::need(
      base::any(!base::is.na(results$Latin_Name)),
      base::paste0("No ", base::switch(input$tsGroup,
                                       trees = "tree", saplings = "sapling", seedlings = "tree seedling",
                                       shrubs = "shrub", shseedlings = "shrub seedling",
                                       herbs = "understory plant", vines = "vine", input$tsGroup),
                   " observations were recorded at ",
                   if (input$tsPark == "All") "any monitored park" else NPSForVeg::getNames(VEGDATA[[input$tsPark]], "long"),
                   " during the selected cycles.")))
    
    valid_names <- results$Latin_Name[!base::is.na(results$Latin_Name)]
    
    common_lookup <- stats::setNames(
      base::rep(NA_character_,base::nrow(results)),
      base::seq_len(base::nrow(results)))
    
    name_obj <- if (input$tsPark == "All") VEGDATA[[PARKLIST[1]]] else VEGDATA[[input$tsPark]]
    
    if (base::length(valid_names) > 0) {
      looked_up <- fmt_common(base::tryCatch(
        NPSForVeg::getPlantNames(
          object  = name_obj,
          names = valid_names,
          in.style = "Latin",
          out.style = "common"),
        error = function(e) base::rep(NA_character_, base::length(valid_names))))
      common_lookup[!base::is.na(results$Latin_Name)] <- looked_up}
    
    results$Common_Name <- common_lookup
    
    results
  }) %>% shiny::bindCache(input$tsPark, input$tsGroup, input$tsValues, input$tsCycles[1], input$tsCycles[2])
    
  # filtered plot df
  tsDf <- shiny::reactive({
    shiny::req(input$tsGroup, input$tsValues)
    raw <- tsData()
    shiny::req(!base::is.null(raw),base::nrow(raw) > 0)
    
    sp_col <- base::ifelse(base::isTRUE(input$tsCommon), "Common_Name", "Latin_Name")
    label_col <- base::ifelse(base::isTRUE(input$tsCommon), "Latin_Name",  "Common_Name")
    
    # all cycles present in raw, including placeholder rows
    all_cycles <- raw %>% dplyr::distinct(Cycle, CycleLabel)
    
    if (base::identical(input$tsSpeciesType, "Pick")) {
      shiny::req(input$tsSpecies)
      
      df <- raw %>% dplyr::filter(Latin_Name %in% input$tsSpecies)
      
      # re-add any cycles that disappeared after species filter
      present_cycles <- df %>% dplyr::distinct(Cycle)
      missing_cycles <- dplyr::anti_join(all_cycles, present_cycles, by = "Cycle")
      
      if (base::nrow(missing_cycles) > 0) {
        fill_rows <- tidyr::crossing(Latin_Name = input$tsSpecies, missing_cycles) %>%
          dplyr::mutate(
            Mean = NA_real_, 
            Lower.95 = NA_real_, 
            Upper.95 = NA_real_, 
            Common_Name = NA_character_)
        df <- dplyr::bind_rows(df, fill_rows)}
      
      df <- df %>%
        dplyr::mutate(
          Species = dplyr::if_else(base::is.na(Latin_Name), NA_character_, fmt_common(.data[[sp_col]])),
          LabelOpp = dplyr::if_else(base::is.na(Latin_Name), NA_character_, fmt_common(.data[[label_col]])))
      
    } else if (base::identical(input$tsSpeciesType, "Common")) {
      shiny::req(input$tsTop)
      
      top_sp <- raw %>%
        dplyr::filter(!base::is.na(Latin_Name)) %>%
        dplyr::group_by(Latin_Name) %>%
        dplyr::summarise(OverallMean = base::mean(Mean, na.rm = TRUE), .groups = "drop") %>%
        dplyr::slice_max(OverallMean, n = input$tsTop, with_ties = FALSE) %>%
        dplyr::pull(Latin_Name)
      
      df <- raw %>% dplyr::filter(Latin_Name %in% top_sp)
      
      present_cycles <- df %>% dplyr::distinct(Cycle)
      missing_cycles <- dplyr::anti_join(all_cycles, present_cycles, by = "Cycle")
      
      if (base::nrow(missing_cycles) > 0) {
        fill_rows <- tidyr::crossing(Latin_Name = top_sp, missing_cycles) %>%
          dplyr::mutate(
            Mean = NA_real_,
            Lower.95 = NA_real_,
            Upper.95 = NA_real_,
            Common_Name = NA_character_)
        df <- dplyr::bind_rows(df, fill_rows)}
      
      df <- df %>%
        dplyr::mutate(
          Species = dplyr::if_else(base::is.na(Latin_Name), NA_character_, fmt_common(.data[[sp_col]])),
          LabelOpp = dplyr::if_else(base::is.na(Latin_Name), NA_character_, fmt_common(.data[[label_col]])))
      
    } else if (base::identical(input$tsSpeciesType, "Least")) {
      shiny::req(input$tsTop)
      
      bottom_sp <- raw %>%
        dplyr::filter(!base::is.na(Latin_Name)) %>%
        dplyr::group_by(Latin_Name) %>%
        dplyr::summarise(OverallMean = base::mean(Mean, na.rm = TRUE), .groups = "drop") %>%
        dplyr::slice_min(OverallMean, n = input$tsTop, with_ties = FALSE) %>%
        dplyr::pull(Latin_Name)
      
      df <- raw %>% dplyr::filter(Latin_Name %in% bottom_sp)
      
      present_cycles <- df %>% dplyr::distinct(Cycle)
      missing_cycles <- dplyr::anti_join(all_cycles, present_cycles, by = "Cycle")
      
      if (base::nrow(missing_cycles) > 0) {
        fill_rows <- tidyr::crossing(Latin_Name = bottom_sp, missing_cycles) %>%
          dplyr::mutate(
            Mean = NA_real_,
            Lower.95 = NA_real_,
            Upper.95 = NA_real_,
            Common_Name = NA_character_)
        df <- dplyr::bind_rows(df, fill_rows)}
      
      df <- df %>%
        dplyr::mutate(
          Species = dplyr::if_else(base::is.na(Latin_Name), NA_character_, fmt_common(.data[[sp_col]])),
          LabelOpp = dplyr::if_else(base::is.na(Latin_Name), NA_character_, fmt_common(.data[[label_col]])))
      
    } else {
      # All species combined
      agg_fun <- if (input$tsValues %in% base::c("count", "size")) sum else mean
      
      df <- raw %>%
        dplyr::group_by(Cycle, CycleLabel) %>%
        dplyr::summarise(
          Latin_Name  = "All species",
          Common_Name = "All species",
          Mean = if (all(base::is.na(Mean))) NA_real_ else agg_fun(Mean, na.rm = TRUE),
          Lower.95 = if (all(base::is.na(Lower.95))) NA_real_ else agg_fun(Lower.95, na.rm = TRUE),
          Upper.95 = if (all(base::is.na(Upper.95))) NA_real_ else agg_fun(Upper.95, na.rm = TRUE),
          .groups = "drop") %>%
        dplyr::mutate(Species  = "All species", LabelOpp = "All species")}
    
    df <- df %>% dplyr::arrange(Cycle)
    df})

  # plot
  output$tsPlot <- plotly::renderPlotly({
    shiny::validate(shiny::need(
        !base::is.null(input$tsPark) && base::nzchar(input$tsPark),
        "There are no results for this combination of choices. Please select a park, species, or plant type."))
    shiny::validate(shiny::need(
      !(base::identical(input$tsSpeciesType, "Pick") &&
          (base::is.null(input$tsSpecies) || base::length(input$tsSpecies) == 0)),
      "There are no results for this combination of choices. Please select a park, species, or plant type."))
    shiny::req(tsDf())

    df <- tsDf()
    font_s <- base::ifelse(base::is.null(input$tsFontSize), 12,  input$tsFontSize)
    lw <- base::ifelse(base::is.null(input$tsLineThickness), 1.5, input$tsLineThickness)
    ribbon_op <- base::ifelse(base::is.null(input$tsRibbonOpacity), 0.2, input$tsRibbonOpacity)

    species_list <- base::unique(df$Species)
    species_list <- species_list[!base::is.na(species_list)]
    cycle_labels <- df %>%
      dplyr::distinct(Cycle, CycleLabel) %>%
      dplyr::arrange(Cycle)

    # color palette
    n_sp  <- base::length(species_list)
    pal <- base::switch(
      base::ifelse(base::is.null(input$tsColorPalette), "set2", input$tsColorPalette),
      set2 = RColorBrewer::brewer.pal(base::max(3, base::min(n_sp, 8)), "Set2"),
      set1 = RColorBrewer::brewer.pal(base::max(3, base::min(n_sp, 9)), "Set1"),
      dark2 = RColorBrewer::brewer.pal(base::max(3, base::min(n_sp, 8)), "Dark2"),
      paired = RColorBrewer::brewer.pal(base::max(3, base::min(n_sp, 12)), "Paired"),
      RColorBrewer::brewer.pal(base::max(3, base::min(n_sp, 8)), "Set2"))

    pal_hex <- base::sapply(pal, toHex)
    
    # on small screens, show top 5 species by overall mean
    ts_small_screen <- !base::is.null(input$screenW) && input$screenW < 1386
    top5_species <- df %>%
      dplyr::filter(!base::is.na(Mean), !base::is.na(Species), Species != "All species") %>%
      dplyr::group_by(Species) %>%
      dplyr::summarise(OverallMean = base::mean(Mean, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(OverallMean)) %>%
      dplyr::slice_head(n = 5) %>%
      dplyr::pull(Species) %>%
      base::as.character()
    legend_species <- if (ts_small_screen) top5_species else species_list
    
    p <- plotly::plot_ly()
    
    # assign colors to species
    for (i in base::seq_along(species_list)) {
      sp  <- species_list[[i]]
      col <- pal_hex[[(i - 1) %% base::length(pal_hex) + 1]]
      d <- df %>% dplyr::filter(Species == sp) %>% dplyr::arrange(Cycle)
      d <- d %>%
        dplyr::mutate(ci_valid = !(base::is.na(Lower.95) |
                                     base::is.na(Upper.95) |
                                     Lower.95 == Upper.95 |
                                     (Lower.95 == Mean & Upper.95 == Mean)),
          
          Lower_95_disp = dplyr::if_else(ci_valid, Lower.95, NA_real_),
          Upper_95_disp = dplyr::if_else(ci_valid, Upper.95, NA_real_))
      d <- d %>%
        dplyr::mutate(
          hover_txt = base::sprintf("<b>%s</b><br>(%s)<br>Mean: %.2f<br>Lower 95%%: %s<br>Upper 95%%: %s", Species, LabelOpp, Mean,
                                    base::ifelse(base::is.na(Lower_95_disp), "NA", base::sprintf("%.2f", Lower_95_disp)),
                                    base::ifelse(base::is.na(Upper_95_disp), "NA", base::sprintf("%.2f", Upper_95_disp))))
      d <- d %>%
        dplyr::mutate(Mean_plot = Mean + (i * 0.0005))
      
      # CI ribbons
      rgb_vals <- grDevices::col2rgb(col)
      ribbon_col <- base::sprintf("rgba(%d,%d,%d,%.2f)", rgb_vals[1], rgb_vals[2], rgb_vals[3], ribbon_op)

      # add confidence intervals if requested
      if (base::isTRUE(input$tsShowCI)) {
        
        # CI ribbons
        p <- plotly::add_trace(
          p,
          data = d,
          x = ~CycleLabel,
          y = ~Upper.95,
          type = "scatter",
          mode = "lines",
          line = base::list(color = "transparent"),
          showlegend = FALSE,
          hoverinfo = "skip",
          name = base::paste0(sp, "_upper"),
          legendgroup = sp)
        
        p <- plotly::add_trace(
          p,
          data = d,
          x = ~CycleLabel,
          y = ~Lower.95,
          type = "scatter",
          mode = "lines",
          fill = "tonexty",
          fillcolor = ribbon_col,
          line = base::list(color = "transparent"),
          showlegend = FALSE,
          hoverinfo = "skip",
          name = base::paste0(sp, "_lower"),
          legendgroup = sp)}

      # mean
      p <- plotly::add_trace(p,
                             data = d,
                             x = ~CycleLabel,
                             y = ~Mean_plot,
                             type = "scatter",
                             mode = "lines+markers",
                             line = base::list(color = col, width = lw),
                             showlegend = sp %in% legend_species,
                             marker = base::list(color = col, size = 6),
                             name = sp,
                             legendgroup = sp,
                             hovertext = ~hover_txt,
                             hoverinfo = "text",
                             hoverlabel = base::list(
                               bgcolor = "white",
                               bordercolor = col,
                               font = base::list(color = "black")))}

    # title
    park_label <- if (input$tsPark == "All") "All Parks" else NPSForVeg::getNames(VEGDATA[[input$tsPark]], "long")
    group_label <- base::switch(input$tsGroup,
                                trees = "Tree",
                                saplings = "Sapling",
                                seedlings = "Tree Seedling",
                                shrubs = "Shrub",
                                shseedlings = "Shrub Seedling",
                                herbs = "Understory Plant",
                                vines = "Vines on Trees",
                                cwd = "Coarse Woody Debris")
    val_label <- base::switch(input$tsValues,
                              count = "Abundance",
                              size = base::switch(input$tsGroup, trees =, saplings = "Basal Area",herbs = "Percent Cover", cwd = "Volume"),
                              presab = "Proportion of Plots Occupied")
    ts_title <- base::paste0(park_label, ": ", group_label, " ", val_label, " by Cycle")
    ts_container_w <- if (!base::is.null(input$tsPlotContainer_width)) input$tsPlotContainer_width else input$screenW
    plotCfg <- buildPlotlyTitleLegend(ts_title, font_s, ts_container_w, legend_species, smooth_breakpoint = TRUE)
    
    #### dummy trace to show legend when there is only one trace (one species selected or all species) --- warning will show Ignoring 1 observations - nothing from data is actually dropped
    p <- plotly::add_trace(
      p,
      x = NA,
      y = NA,
      type = "scatter",
      mode = "markers",
      name = " ",
      showlegend = TRUE,
      hoverinfo = "skip",
      marker = base::list(opacity = 0))

    p <- plotly::layout(p,
                        xaxis = base::list(
                          title = base::list(text = "Monitoring Cycle", font = base::list(size = font_s)),
                          type = "category",
                          categoryorder = "array",
                          categoryarray = cycle_labels$CycleLabel,
                          tickfont = base::list(size = font_s - 2),
                          tickmode = "array",
                          tickvals = cycle_labels$CycleLabel,
                          ticktext = cycle_labels$CycleLabel,
                          showgrid = TRUE),
                        yaxis = base::list(
                          title = base::list(text = tsYlabel(), font = base::list(size = font_s)),
                          tickfont = base::list(size = font_s),
                          rangemode = "tozero"),
                        title = plotCfg$title,
                        showlegend = TRUE,
                        legend = plotCfg$legend,
                        hovermode = "closest",
                        margin = plotCfg$margin,
                        height = plotCfg$height,
                        font = base::list(size = font_s),
                        autosize  = TRUE)
    
    p <- p %>% plotly::config(
      displayModeBar = base::is.null(input$screenW) || input$screenW >= 768,
      displaylogo = FALSE)
    p})
  
  output$TSLimitWarning <- shiny::renderUI({
    shiny::req(
      !base::is.null(input$tsPark) && base::nzchar(input$tsPark),
      !(base::identical(input$tsSpeciesType, "Pick") &&
          (base::is.null(input$tsSpecies) || base::length(input$tsSpecies) == 0)))
    sw <- input$screenW
    if (base::is.null(sw) || sw >= 1386) base::return(NULL)
    htmltools::tags$div(style = "font-size: 12px; color: #888; font-style: italic; margin-top: 6px; text-align: center;",
                        "* Note: On smaller screens, the legend shows only the top 5 species by overall mean. To view all species details, click or hover on a point. To view the full list, open the summary report or data table.")})

  #highlight plotly item on click
  tsSelectedSpecies <- shiny::reactiveVal(NULL)
  
  # table title
  tsTitleText <- shiny::reactive({
    shiny::req(input$tsPark, input$tsGroup, input$tsValues, input$tsCycles)
    shiny::req(base::nzchar(input$tsPark))
    
    park_label <- if (input$tsPark == "All") "All Parks" else NPSForVeg::getNames(VEGDATA[[input$tsPark]], "long")
    group_label <- base::switch(input$tsGroup,
                                trees = "Tree",
                                saplings = "Sapling",
                                seedlings = "Tree Seedling",
                                shrubs = "Shrub",
                                shseedlings = "Shrub Seedling",
                                herbs = "Understory Plant",
                                vines = "Vines on Trees")
    
    val_label <- base::switch(input$tsValues,
                              count  = "Abundance",
                              size = base::switch(input$tsGroup, trees =, saplings = "Basal Area", herbs = "Percent Cover", cwd = "Volume"),
                              presab = "Proportion of Plots Occupied")
    
    all_cycles <- base::sort(base::unique(DATACYCLES$Cycle))
    cycles_use <- all_cycles[all_cycles >= input$tsCycles[1] & all_cycles <= input$tsCycles[2]]
    
    yr_start <- base::min(DATACYCLES$YearStart[DATACYCLES$Cycle %in% cycles_use])
    yr_end <- base::max(DATACYCLES$YearEnd[DATACYCLES$Cycle %in% cycles_use])
    period <- base::paste0(yr_start, "-", yr_end)
    
    unit_suffix <- base::paste0(" (", tsYlabel(), ")")
    
    base::paste0(park_label, ": ", group_label, " ", val_label, " by Cycle ", period, unit_suffix)})

  output$tsTableTitle <- shiny::renderText({ tsTitleText() })

  # table
  tempTsTable <- shiny::reactive({
    shiny::req(tsDf())
    full_df <- tsDf()
    shiny::validate(shiny::need(!base::is.null(input$tsPark) && input$tsPark != "", 
                                "There are no results for this combination of choices. Please select a park, species, or plant type."))
    
    
    df <- full_df %>%
      dplyr::select(Species, CycleLabel, Mean, Lower.95, Upper.95) %>%
      dplyr::filter(!base::is.na(Species), !base::is.na(Mean)) %>%
      dplyr::mutate(`Lower 95% CI` = dplyr::if_else(base::is.na(Lower.95) | Lower.95 == Mean | (Lower.95 == Mean & Upper.95 == Mean), NA_real_, Lower.95),
                    `Upper 95% CI` = dplyr::if_else(base::is.na(Upper.95) | Upper.95 == Mean | (Lower.95 == Mean & Upper.95 == Mean), NA_real_,Upper.95),
                    Mean = base::sprintf("%.2f", Mean),
                    `Lower 95% CI` = base::ifelse(base::is.na(`Lower 95% CI`), "NA", base::sprintf("%.2f", `Lower 95% CI`)),
                    `Upper 95% CI` = base::ifelse(base::is.na(`Upper 95% CI`), "NA", base::sprintf("%.2f", `Upper 95% CI`))) %>%
      dplyr::rename(Cycle = CycleLabel) %>%
      dplyr::select(-Lower.95, -Upper.95)
    
    if (base::identical(input$tsTableOrder, "cycle")) {
      species_means <- full_df %>%
        dplyr::filter(!base::is.na(Mean)) %>%
        dplyr::group_by(Species) %>%
        dplyr::summarise(OverallMean = mean(Mean, na.rm = TRUE), .groups = "drop")
      
      df <- df %>%
        dplyr::left_join(species_means, by = "Species") %>%
        dplyr::arrange(Cycle, dplyr::desc(OverallMean)) %>%
        dplyr::select(-OverallMean) %>%
        dplyr::group_by(Cycle) %>%
        dplyr::mutate(Cycle = dplyr::if_else(dplyr::row_number() == 1, Cycle, "")) %>%
        dplyr::ungroup() %>%
        dplyr::select(Cycle, Species, Mean, `Lower 95% CI`, `Upper 95% CI`)
    } else {
      
      species_order <- full_df %>%
        dplyr::filter(!base::is.na(Mean)) %>%
        dplyr::group_by(Species) %>%
        dplyr::summarise(OverallMean = mean(Mean, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(OverallMean)) %>%
        dplyr::pull(Species)
      
      df <- df %>%
        dplyr::mutate(Species_factor = factor(Species, levels = species_order)) %>%
        dplyr::arrange(Species_factor, Cycle) %>%
        dplyr::select(-Species_factor) %>%
        dplyr::group_by(Species) %>%
        dplyr::mutate(Species = dplyr::if_else(dplyr::row_number() == 1, Species, "")) %>%
        dplyr::ungroup()}
    
    df})

  output$tsTable <- DT::renderDataTable({
    
    if (base::identical(input$tsSpeciesType, "Pick") && (base::is.null(input$tsSpecies) || base::length(input$tsSpecies) == 0)) {base::return(NULL)}
    
    DT::datatable(
      tempTsTable(),
      rownames = FALSE,
      options = base::list(dom = "t", pageLength = -1, ordering = FALSE, columnDefs = base::list(list(className = "dt-left", targets = "_all"))))})

  # table download
  output$tsTableDownload <- shiny::downloadHandler(
    filename = function() {base::paste0(base::gsub("[^A-Za-z0-9_\\-]+", "_", tsTitleText()), ".csv")},
    content = function(file) {
      df <- tempTsTable()
      shiny::req(!base::is.null(df), base::nrow(df) > 0)
      utils::write.csv(df, file, row.names = FALSE)})

  # missing data warning
  tsMissingWarningDismissed <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(base::list(input$tsPark, input$tsGroup, input$tsCycles, input$tsValues),
    { tsMissingWarningDismissed(FALSE) })

  shiny::observeEvent(input$dismiss_ts_warning, { tsMissingWarningDismissed(TRUE) })

  tsMissingWarningData <- shiny::reactive({
    shiny::req(tsData())
    raw <- tsData()
    
    all_cycles <- raw %>% dplyr::distinct(Cycle, CycleLabel)
    
    # cycles where every row is a placeholder (no species observed at all)
    dropped <- raw %>%
      dplyr::group_by(Cycle, CycleLabel) %>%
      dplyr::summarise(all_na = all(base::is.na(Latin_Name)), .groups = "drop") %>%
      dplyr::filter(all_na)
    
    if (base::nrow(dropped) == 0) base::return(NULL)
    dropped$CycleLabel})

  tsMissingWarningMsg <- shiny::reactive({
    missing <- tsMissingWarningData()
    if (base::is.null(missing)) base::return(NULL)
    
    park_label <- if (input$tsPark == "All") "All Parks" else NPSForVeg::getNames(VEGDATA[[input$tsPark]], "long")
    group_label <- base::switch(input$tsGroup,
                                trees = "tree",
                                saplings = "sapling",
                                seedlings = "tree seedling",
                                shrubs = "shrub",
                                shseedlings = "shrub seedling",
                                herbs = "understory plant",
                                vines = "vine",
                                input$tsGroup)
    
    n <- base::length(missing)
    
    base::paste0("Warning: No ", group_label, " observations were recorded at ", park_label, " during: ", base::paste(missing, collapse = "; "),
                 ". ", if (n == 1) "This cycle has" else "These cycles have", " been removed from the figure and table.")})
  
  tsSinglePlotWarningMsg <- shiny::reactive({
    shiny::req(input$tsPark, base::nzchar(input$tsPark))
    if (input$tsPark == "All") base::return(NULL)
    shiny::req(tsData())
    df <- tsData()
    
    single_plot_cycles <- df %>%
      dplyr::filter(
        !base::is.na(Mean),
        !base::is.na(Lower.95),
        !base::is.na(Upper.95),
        Lower.95 == Mean,
        Upper.95 == Mean) %>%
      dplyr::distinct(CycleLabel) %>%
      dplyr::pull(CycleLabel)
    
    if (base::length(single_plot_cycles) == 0) base::return(NULL)
    
    base::paste0("Warning: Only one monitoring plot exists for ", base::paste(single_plot_cycles, collapse = "; "),". Confidence intervals cannot be estimated.")})
  
  tsSinglePlotWarningDismissed <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(
    base::list(input$tsPark,
         input$tsGroup,
         input$tsCycles,
         input$tsValues),
    {tsSinglePlotWarningDismissed(FALSE)})
  
   # summary report
   tsReportText <- shiny::reactive({
    shiny::req(input$tsPark, input$tsGroup, input$tsValues, input$tsCycles)
    shiny::req(base::nzchar(input$tsPark))
    df <- base::tryCatch(tsDf(), error = function(e) NULL)
    if (base::is.null(df) || base::nrow(df) == 0) base::return(NULL)
    
    park_label <- if (input$tsPark == "All") "All NCRN parks" else NPSForVeg::getNames(VEGDATA[[input$tsPark]], "long")
    group_label <- base::switch(input$tsGroup,
                                trees = "trees", saplings = "saplings", seedlings = "tree seedlings",
                                shrubs = "shrubs", shseedlings = "shrub seedlings",
                                herbs = "understory plants", vines = "vines", input$tsGroup)
    val_label <- base::switch(input$tsValues,
                              count = base::switch(input$tsGroup,
                                                   trees = "trees per hectare", saplings = "saplings per hectare",
                                                   seedlings = "tree seedlings per hectare", shrubs = "shrubs per hectare",
                                                   shseedlings = "shrub seedlings per hectare", vines = "vines per hectare",
                                                   "individuals per hectare"),
                              size  = base::switch(input$tsGroup,
                                                   trees =, saplings = "square meters of basal area per hectare",
                                                   herbs = "percent cover","size units per hectare"),
                              presab = "proportion of plots occupied")
    
    all_c <- base::sort(base::unique(DATACYCLES$Cycle))
    cycles_use <- all_c[all_c >= input$tsCycles[1] & all_c <= input$tsCycles[2]]
    yr_start <- base::min(DATACYCLES$YearStart[DATACYCLES$Cycle %in% cycles_use])
    yr_end <- base::max(DATACYCLES$YearEnd[DATACYCLES$Cycle   %in% cycles_use])
    period <- base::paste0(yr_start, "\u2013", yr_end)
    n_cycles <- base::length(cycles_use)
    cycle_word <- if (n_cycles == 1) "cycle" else "cycles"
    
    # per-species summary across cycles
    df_valid <- df %>% dplyr::filter(!base::is.na(Mean), !base::is.na(Species))
    
    species_order <- df_valid %>%
      dplyr::filter(Species != "All species") %>%
      dplyr::group_by(Species) %>%
      dplyr::summarise(OverallMean = base::mean(Mean, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(OverallMean)) %>%
      dplyr::pull(Species)
    
    species_list <- base::as.character(species_order)
    if (base::identical(input$tsSpeciesType, "All")) species_list <- "all species combined"
    n_sp <- base::length(species_list)
    
    bullet_tags <- if (!base::identical(input$tsSpeciesType, "All") && n_sp > 0) {
      base::lapply(species_list, function(sp) {sp_df <- df_valid %>% dplyr::filter(Species == sp) %>% dplyr::arrange(Cycle)
        if (base::nrow(sp_df) == 0) {base::return(htmltools::tags$li(htmltools::tags$strong(sp), " \u2014 no observations recorded."))}
        overall_mean <- base::round(base::mean(sp_df$Mean, na.rm = TRUE), 2)
        opp <- if (!base::is.null(sp_df$LabelOpp) && !base::is.na(sp_df$LabelOpp[1]) && base::nzchar(sp_df$LabelOpp[1])) {base::paste0(" (", sp_df$LabelOpp[1], ")")
        } else { "" }
        trend_str <- if (base::nrow(sp_df) >= 2) {first_mean <- sp_df$Mean[1]; last_mean <- sp_df$Mean[base::nrow(sp_df)]
        percent_change <- base::round(((last_mean - first_mean) / base::max(first_mean, 0.0001)) * 100, 1)
        direction <- if (last_mean > first_mean) "increased" else if (last_mean < first_mean) "decreased" else "remained stable"
        base::sprintf("%s by %.1f%% from %s to %s",
                      direction, base::abs(percent_change),
                      base::as.character(sp_df$CycleLabel[1]),
                      base::as.character(sp_df$CycleLabel[base::nrow(sp_df)]))
        } else { "only one cycle" }
        htmltools::tags$li(htmltools::tags$strong(sp), opp, base::sprintf(" \u2014 Overall mean: %.2f %s | Trend: %s.", overall_mean, val_label, trend_str))})
    } else {
      sp_df <- df_valid %>% dplyr::arrange(Cycle)
      overall_mean <- base::round(base::mean(sp_df$Mean, na.rm = TRUE), 2)
      trend_str <- if (base::nrow(sp_df) >= 2) {first_mean <- sp_df$Mean[1]; last_mean <- sp_df$Mean[base::nrow(sp_df)]
      percent_change <- base::round(((last_mean - first_mean) / base::max(first_mean, 0.0001)) * 100, 1)
      direction <- if (last_mean > first_mean) "increased" else if (last_mean < first_mean) "decreased" else "remained stable"
      base::sprintf("%s by %.1f%% from %s to %s",
                    direction, base::abs(percent_change),
                    base::as.character(sp_df$CycleLabel[1]),
                    base::as.character(sp_df$CycleLabel[base::nrow(sp_df)]))
      } else { "only one cycle" }
      base::list(htmltools::tags$li(htmltools::tags$strong("All species combined"), base::sprintf(" \u2014 Overall mean: %.2f %s | Trend: %s.", overall_mean, val_label, trend_str)))}
    
    intro <- if (base::identical(input$tsSpeciesType, "All")) {
      base::sprintf("Showing all species combined for %s across %d monitoring %s (%s) at %s.", group_label, n_cycles, cycle_word, period, park_label)
    } else {base::sprintf("Showing %d species of %s across %d monitoring %s (%s) at %s.", n_sp, group_label, n_cycles, cycle_word, period, park_label)}
    
    htmltools::tagList(htmltools::tags$p(htmltools::tags$span(style = "font-size: 30px; font-weight: bold;", "Summary Report:")),
      htmltools::tags$p(style = "font-size: 15px;",
        htmltools::tags$strong(style = "font-size: 20px;", park_label), " \u2014 ", base::trimws(intro)),
      htmltools::tags$ul(style = "font-size: 15px; margin: 6px 0 6px 16px; padding: 0;", bullet_tags),
      htmltools::tags$p(style = "font-size: 15px;",
        htmltools::tags$em("Overall means averaged across all cycles shown; trend compares first to last displayed cycle.")))})
  
  output$tsReport <- shiny::renderUI({
    txt <- base::tryCatch(tsReportText(), error = function(e) NULL)
    if (base::is.null(txt)) base::return(NULL)
    htmltools::tagList(
      htmltools::tags$div(
        style = "margin-bottom: 12px;",
        htmltools::tags$div(
          class = "report-header",
          style = "cursor: pointer; font-size: 13px; font-weight: bold; padding: 8px 12px;
               background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px;
               user-select: none; display: flex; align-items: center; gap: 6px;",
          htmltools::tags$span(class = "report-caret", "\u25bc"),
          "Summary Report"
        ),
        htmltools::tags$div(
          class = "report-body",
          style = "padding: 0 16px; background-color: #f8f9fa;
               border: 1px solid #dee2e6; border-top: none;
               border-radius: 0 0 6px 6px; font-size: 13px; line-height: 1.8;",
          txt)))
  })
  
  output$tsReportTable <- shiny::renderUI({
    txt <- base::tryCatch(tsReportText(), error = function(e) NULL)
    if (base::is.null(txt)) base::return(NULL)
    htmltools::tagList(
      htmltools::tags$div(
        style = "margin-bottom: 12px;",
        htmltools::tags$div(
          class = "report-header",
          style = "cursor: pointer; font-size: 13px; font-weight: bold; padding: 8px 12px;
               background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px;
               user-select: none; display: flex; align-items: center; gap: 6px;",
          htmltools::tags$span(class = "report-caret", "\u25bc"),
          "Summary Report"
        ),
        htmltools::tags$div(
          class = "report-body",
          style = "padding: 0 16px; background-color: #f8f9fa;
               border: 1px solid #dee2e6; border-top: none;
               border-radius: 0 0 6px 6px; font-size: 13px; line-height: 1.8;",
          txt)))
  })
  
  shiny::observeEvent(
    input$dismiss_ts_singleplot_warning,
    {tsSinglePlotWarningDismissed(TRUE)})
  output$tsMissingWarning <- shiny::renderUI({
    if (tsMissingWarningDismissed()) base::return(NULL)
    msg <- tsMissingWarningMsg()
    if (base::is.null(msg)) base::return(NULL)
    htmltools::tags$div(
      style = "padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
      msg,
      htmltools::tags$button("\u00d7", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                             onclick = "Shiny.setInputValue('dismiss_ts_warning', Math.random())"))})
  
  output$tsMissingWarningTable <- shiny::renderUI({
    if (tsMissingWarningDismissed()) base::return(NULL)
    msg <- tsMissingWarningMsg()
    if (base::is.null(msg)) base::return(NULL)
    htmltools::tags$div(
      style = "padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
      msg,
      htmltools::tags$button("\u00d7", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                             onclick = "Shiny.setInputValue('dismiss_ts_warning', Math.random())"))})
  
  output$tsSinglePlotWarning <- shiny::renderUI({
    if (tsSinglePlotWarningDismissed()) base::return(NULL)
    msg <- tsSinglePlotWarningMsg()
    if (base::is.null(msg)) base::return(NULL)
    
    htmltools::tags$div(style = "padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
      msg,
      htmltools::tags$button("\u00d7", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                             onclick = "Shiny.setInputValue('dismiss_ts_singleplot_warning', Math.random())"))})
 
   output$tsSinglePlotWarningTable <- shiny::renderUI({
    if (tsSinglePlotWarningDismissed()) base::return(NULL)
    msg <- tsSinglePlotWarningMsg()
    if (base::is.null(msg)) base::return(NULL)
    
    htmltools::tags$div(style = "padding: 10px 14px; margin-bottom: 10px; border: 1px solid #f5c2c7; background-color: #f8d7da; color: #842029; border-radius: 6px; position: relative;",
      msg,
      htmltools::tags$button("\u00d7", style = "position: absolute; right: 10px; top: 5px; border: none; background: none; font-size: 18px; cursor: pointer;",
                             onclick = "Shiny.setInputValue('dismiss_ts_singleplot_warning', Math.random())"))})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
                 Least = {max_sp <- IVSpeciesCount()
                 htmltools::tags$div(title = "Select the maximum number of species to plot",
                                     shiny::sliderInput("IVTop", "Maximum number of species to plot (least common, in order of IV):",
                                                        min = 1, max = base::max(1, max_sp, na.rm = TRUE), value = base::min(5, max_sp), step = 1, ticks = FALSE))},
                 Pick = if (base::is.null(input$IVPark) || base::nchar(input$IVPark) == 0) {base::return()
                 } else {htmltools::tags$div(title = "Click here to pick the species you want to graph",
                                             shiny::selectizeInput(inputId = "IVSpecies", label = "Select one or more species",
                                                                   choices = IVSpecList(), multiple = TRUE, selected = input$IVSpecies,
                                                                   options = base::list(placeholder='Select a species to display',
                                                                                        plugins = base::list("remove_button"))))},
                 All = NULL)})
  
  # iv reset buttons
  shiny::observeEvent(input$IVResetData, {
    shiny::updateSelectizeInput(session, "IVPark", selected = "")
    shiny::updateSelectInput(session, "IVCycles",
                             selected = base::as.character(DATACYCLES$Cycle[base::nrow(DATACYCLES)]))
    shiny::updateSelectizeInput(session, "IVGroup", selected = IVPLANTTYPES[[1]])
    shiny::updateCheckboxInput(session, "IVCommon", value = TRUE)
    shiny::updateCheckboxInput(session, "IVPlotlyText", value = FALSE)
    shiny::updateCheckboxInput(session, "IVPart", value = FALSE)
    shiny::updateRadioButtons(session, "IVSpeciesType", selected = "Common")
    shiny::updateSelectizeInput(session, "IVSpecies", selected = base::character(0))
  })
  
  shiny::observeEvent(input$IVResetDisplay, {
    shiny::updateSelectizeInput(session, "IVBaseColor", selected = "green4")
    shiny::updateSliderInput(session, "IVFontSize", value = 12)
    shiny::updateSelectizeInput(session, "IVDensityColor",
                                selected = if ("green4" %in% COLORNAMES) "green4" else COLORNAMES[[1]])
    shiny::updateSelectizeInput(session, "IVSizeColor",
                                selected = if ("chartreuse" %in% COLORNAMES) "chartreuse" else COLORNAMES[[1]])
    shiny::updateSelectizeInput(session, "IVDistributionColor",
                                selected = if ("yellow" %in% COLORNAMES) "yellow" else COLORNAMES[[1]])
  })
  
  IVDataIsDefault <- shiny::reactive({
    default_cycle <- base::as.character(DATACYCLES$Cycle[base::nrow(DATACYCLES)])
    
    (base::is.null(input$IVPark)        || base::identical(input$IVPark, "")) &&
      (base::is.null(input$IVCycles)      || base::identical(input$IVCycles, default_cycle)) &&
      (base::is.null(input$IVGroup)       || base::identical(input$IVGroup, IVPLANTTYPES[[1]])) &&
      (base::is.null(input$IVCommon)      || base::isTRUE(input$IVCommon)) &&
      (base::is.null(input$IVPlotlyText)  || !base::isTRUE(input$IVPlotlyText)) &&
      (base::is.null(input$IVPart)        || !base::isTRUE(input$IVPart)) &&
      (base::is.null(input$IVSpeciesType) || base::identical(input$IVSpeciesType, "Common")) &&
      (base::is.null(input$IVSpecies)     || base::length(input$IVSpecies) == 0)
  })
  
  shiny::observe({
    shinyjs::toggleClass(id = "IVResetData", class = "btn-danger",
                         condition = !IVDataIsDefault())
  })
  
  IVDisplayIsDefault <- shiny::reactive({
    (base::is.null(input$IVBaseColor)         || base::identical(input$IVBaseColor, "green4")) &&
      (base::is.null(input$IVFontSize)          || input$IVFontSize == 12) &&
      (base::is.null(input$IVDensityColor)      || base::identical(input$IVDensityColor, if ("green4" %in% COLORNAMES) "green4" else COLORNAMES[[1]])) &&
      (base::is.null(input$IVSizeColor)         || base::identical(input$IVSizeColor, if ("chartreuse" %in% COLORNAMES) "chartreuse" else COLORNAMES[[1]])) &&
      (base::is.null(input$IVDistributionColor) || base::identical(input$IVDistributionColor, if ("yellow" %in% COLORNAMES) "yellow" else COLORNAMES[[1]]))
  })
  
  shiny::observe({
    shinyjs::toggleClass(id = "IVResetDisplay", class = "btn-danger",
                         condition = !IVDisplayIsDefault())
  })
  
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
      NPSForVeg::getNames(VEGDATA[[input$IVPark]], "long"), ": ",
      IVTitleGroup(), " Importance Values ",
      base::min(IVYears()), "-", base::max(IVYears()))})
  
  #### IV Data ####
  
  IVData <- shiny::reactive({
    shiny::req(input$IVPark, base::nchar(input$IVPark) > 0)
    shiny::req(IVYears())
    
    # get latin names
    raw_latin <- base::tryCatch(
      NPSForVeg::IV(
        object = VEGDATA[[input$IVPark]],
        group = input$IVGroup,
        years = IVYears(),
        common = FALSE), error = function(e) NULL)
    
    shiny::validate(
      shiny::need(!base::is.null(raw_latin) &&base::nrow(raw_latin) > 0,
        base::paste0("No ",
          switch(input$IVGroup,
                 trees = "tree",
                 saplings = "sapling",
                 seedlings = "tree seedling",
                 shseedlings = "shrub seedling",
                 input$IVGroup),
          " observations were recorded at ", NPSForVeg::getNames(VEGDATA[[input$IVPark]], "long"), " during ",min(IVYears()), "-", max(IVYears()), ".")))
    
    raw <- NPSForVeg::IV(
      object = VEGDATA[[input$IVPark]],
      group = input$IVGroup,
      years = IVYears(),
      common = input$IVCommon)
    
    raw$Species <- fmt_common(raw$Species)
    
    # LabelOpp = the opposite of whatever Species currently is
    name_map <- base::tryCatch({
      base::data.frame(
        Latin_Name  = raw_latin$Species,
        Common_Name = fmt_common(NPSForVeg::getPlantNames(
          object    = VEGDATA[[input$IVPark]],
          names     = raw_latin$Species,
          in.style  = "Latin",
          out.style = "common")),
        stringsAsFactors = FALSE)
    }, error = function(e) {
      base::data.frame(
        Latin_Name  = raw_latin$Species,
        Common_Name = NA_character_,
        stringsAsFactors = FALSE)})
    
    if (base::isTRUE(input$IVCommon)) {
      raw$LabelOpp <- name_map$Latin_Name[match(raw$Species, name_map$Common_Name)]
    } else {raw$LabelOpp <- name_map$Common_Name[match(raw$Species, name_map$Latin_Name)]}
    
    raw$LabelOpp <- base::ifelse(
      base::is.na(raw$LabelOpp) | !base::nzchar(raw$LabelOpp),
      fmt_common(raw$Species),
      raw$LabelOpp)

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
  
  #### disable component color pickers when components arent shown ####
  shiny::observe({
    show_components <- base::isTRUE(input$IVPart)
    shinyjs::toggleState(id = "IVDensityColor", condition = show_components)
    shinyjs::toggleState(id = "IVSizeColor", condition = show_components)
    shinyjs::toggleState(id = "IVDistributionColor", condition = show_components)})
  
  #### component color disabled notice ####
  ivComponentColorDisabledMsg <- shiny::reactive({
    if (!base::isTRUE(input$IVPart)) {
      htmltools::tags$div(
        style = "font-size: 11px; color: #888; font-style: italic; margin-top: 2px; max-width: 320px; text-align: center;",
        "Component colors are available when \"Display components of the importance value?\" is checked.")
    } else { NULL }})
  
  output$IVComponentColorNotice <- shiny::renderUI({ ivComponentColorDisabledMsg() })
  shiny::outputOptions(output, "IVComponentColorNotice", suspendWhenHidden = FALSE)
  
  #### IV Plot ####
  tempIVPlot <- shiny::reactive({
    shiny::req(IVData())
    
    if (base::is.null(input$IVPark) || base::nchar(input$IVPark) == 0) {
      shiny::validate(shiny::need(
        !base::is.null(IVdf) && base::is.data.frame(IVdf) && base::nrow(IVdf) > 0,
        "There is no data for this combination of choices. Please select a park, species, or plant type."))}
    
    IVdf <- IVData()
    
    # sets number of displayed species
    IVdf <- base::switch(input$IVSpeciesType, 
                         Common = {shiny::req(input$IVTop)
                           IVdf %>%
                             dplyr::slice_max(order_by = Total, n = input$IVTop, with_ties = FALSE) %>%
                             dplyr::arrange(Total)},
                         Least = {shiny::req(input$IVTop)
                           IVdf %>%
                             dplyr::slice_min(order_by = Total, n = input$IVTop, with_ties = FALSE) %>%
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
          bgcolor = "white",
          bordercolor = IVBaseColor(),
          font = base::list(size = 14, color = "black")))
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
          hoverlabel = base::list(
            bgcolor = "white",
            bordercolor = IVDensityColor(),
            font = base::list(size = 14, color = "black"))) %>%
        plotly::add_trace(
          data = IVdf, x = ~Size, y = ~Species,
          name = "Size", type = "bar", orientation = "h",
          legendrank = 2,
          marker = base::list(color = IVSizeColor()),
          text = if (iv_text_on()) {~base::sprintf("%.2f", Size)} else {NULL},
          hovertext = ~base::sprintf("Species: %s<br>Size: %.2f", LabelOpp, Size),
          hoverinfo = "text",
          hoverlabel = base::list(
            bgcolor = "white",
            bordercolor = IVSizeColor(),
            font = base::list(size = 14, color = "black"))) %>%
        plotly::add_trace(
          data = IVdf, x = ~Distribution, y = ~Species,
          name = "Distribution", type = "bar", orientation = "h",
          legendrank = 1,
          marker = base::list(color = IVDistributionColor()),
          text = if (iv_text_on()) {~base::sprintf("%.2f", Distribution)} else {NULL},
          hovertext = ~base::sprintf("Species: %s<br>Distribution: %.2f", LabelOpp, Distribution),
          hoverinfo = "text",
          hoverlabel = base::list(
            bgcolor = "white",
            bordercolor = IVDistributionColor(),
            font = base::list(size = 14, color = "black")))}
    
    IVFontSize <- if (!base::is.null(input$IVFontSize)) input$IVFontSize else 12
    iv_container_w <- if (!base::is.null(input$ivPlotContainer_width)) input$ivPlotContainer_width else input$screenW
    iv_legend_labels <- if (input$IVPart) base::c("Density", "Size", "Distribution") else base::c("Total IV")
    plotCfg <- buildPlotlyTitleLegend(IVTitle(), IVFontSize, iv_container_w, iv_legend_labels)
    
    p <- p %>% plotly::layout(
      barmode = "stack",
      showlegend = TRUE,
      legend = plotCfg$legend,
      title = plotCfg$title,
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
      margin = plotCfg$margin,
      height = plotCfg$height,
      font = base::list(size = IVFontSize),
      autosize = TRUE)
    
    p <- p %>% plotly::config(
      displayModeBar = base::is.null(input$screenW) || input$screenW >= 768,
      displaylogo = FALSE)
    p})
  
  # summary report
  ivReportText <- shiny::reactive({shiny::req(input$IVPark, base::nzchar(input$IVPark), IVYears())
    df_full <- base::tryCatch(IVData(), error = function(e) NULL)
    if (base::is.null(df_full) || base::nrow(df_full) == 0) base::return(NULL)
    
    df <- base::switch(input$IVSpeciesType,
                       Common = {shiny::req(input$IVTop)
                         df_full %>% dplyr::slice_max(order_by = Total, n = input$IVTop, with_ties = FALSE) %>%
                           dplyr::arrange(dplyr::desc(Total))},
                       Least = {shiny::req(input$IVTop)
                         df_full %>% dplyr::slice_min(order_by = Total, n = input$IVTop, with_ties = FALSE) %>%
                           dplyr::arrange(dplyr::desc(Total))},
                       Pick = {shiny::req(input$IVSpecies)
                         df_full %>% dplyr::filter(if (base::isTRUE(input$IVCommon)) LabelOpp %in% input$IVSpecies
                                                   else Species %in% input$IVSpecies) %>%
                           dplyr::arrange(dplyr::desc(Total))},
                       All = df_full %>%
                         dplyr::summarise(
                           Species = "All species", LabelOpp = "All species",
                           Density = base::round(base::mean(Density, na.rm = TRUE), 2),
                           Size = base::round(base::mean(Size, na.rm = TRUE), 2),
                           Distribution = base::round(base::mean(Distribution, na.rm = TRUE), 2),
                           Total = base::round(base::mean(Total, na.rm = TRUE), 2)))
    
    if (base::is.null(df) || base::nrow(df) == 0) base::return(NULL)
    
    park_label <- NPSForVeg::getNames(VEGDATA[[input$IVPark]], "long")
    group_label <- base::switch(input$IVGroup, trees = "trees", saplings = "saplings", seedlings = "tree seedlings", shseedlings = "shrub seedlings", input$IVGroup)
    period <- base::paste0(base::min(IVYears()), "\u2013", base::max(IVYears()))
    n_sp <- base::nrow(df)
    
    intro <- base::sprintf("Showing importance values for %d species of %s at %s from %s.", n_sp, group_label, park_label, period)
    
    top_sentence <- if (n_sp > 1) {
      base::sprintf("%s had the highest importance value (%.2f) and %s had the lowest (%.2f) among the species shown.", base::as.character(df$Species[1]), base::round(df$Total[1], 2),
                    base::as.character(df$Species[n_sp]), base::round(df$Total[n_sp], 2))
      } else { NULL }
    
    bullet_tags <- base::lapply(base::seq_len(n_sp), function(i) {
      sp <- base::as.character(df$Species[i])
      opp <- if (!base::is.null(df$LabelOpp) && !base::is.na(df$LabelOpp[i]) && base::nzchar(df$LabelOpp[i]) && df$LabelOpp[i] != sp) {base::paste0(" (", df$LabelOpp[i], ")")
      } else { "" }
      htmltools::tags$li(htmltools::tags$strong(sp), opp, base::sprintf(" \u2014 Total IV: %.2f | Density: %.2f | Size: %.2f | Distribution: %.2f.",
                                                                        base::round(df$Total[i], 2),
                                                                        base::round(df$Density[i], 2),
                                                                        base::round(df$Size[i], 2),
                                                                        base::round(df$Distribution[i], 2)))})
    
    footnote <- "Importance value (IV) is the sum of relative density (Density), relative size (Size, based on basal area), and relative distribution (Distribution). 
                Each component ranges from 0 to 1; total IV ranges from 0 to 1."
    
    htmltools::tagList(
      htmltools::tags$p(htmltools::tags$span(style = "font-size: 30px; font-weight: bold;", "Summary Report:")),
      htmltools::tags$p(style = "font-size: 15px;", htmltools::tags$strong(style = "font-size: 20px;", park_label), " \u2014 ", intro),
      htmltools::tags$ul(style = "font-size: 15px; margin: 6px 0 6px 16px; padding: 0;", bullet_tags),
      if (!base::is.null(top_sentence)) htmltools::tags$p(style = "font-size: 15px;", top_sentence) else NULL,
      htmltools::tags$p(style = "font-size: 15px;", htmltools::tags$em(footnote)))})
  
  output$ivReport <- shiny::renderUI({
    txt <- base::tryCatch(ivReportText(), error = function(e) NULL)
    if (base::is.null(txt)) base::return(NULL)
    htmltools::tagList(
      htmltools::tags$div(
        style = "margin-bottom: 12px;",
        htmltools::tags$div(
          class = "report-header",
          style = "cursor: pointer; font-size: 13px; font-weight: bold; padding: 8px 12px;
               background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px;
               user-select: none; display: flex; align-items: center; gap: 6px;",
          htmltools::tags$span(class = "report-caret", "\u25bc"),
          "Summary Report"
        ),
        htmltools::tags$div(
          class = "report-body",
          style = "padding: 0 16px; background-color: #f8f9fa;
               border: 1px solid #dee2e6; border-top: none;
               border-radius: 0 0 6px 6px; font-size: 13px; line-height: 1.8;",
          txt)))})
  
  output$ivReportTable <- shiny::renderUI({
    txt <- base::tryCatch(ivReportText(), error = function(e) NULL)
    if (base::is.null(txt)) base::return(NULL)
    htmltools::tagList(
      htmltools::tags$div(
        style = "margin-bottom: 12px;",
        htmltools::tags$div(
          class = "report-header",
          style = "cursor: pointer; font-size: 13px; font-weight: bold; padding: 8px 12px;
               background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px;
               user-select: none; display: flex; align-items: center; gap: 6px;",
          htmltools::tags$span(class = "report-caret", "\u25bc"),
          "Summary Report"
        ),
        htmltools::tags$div(
          class = "report-body",
          style = "padding: 0 16px; background-color: #f8f9fa;
               border: 1px solid #dee2e6; border-top: none;
               border-radius: 0 0 6px 6px; font-size: 13px; line-height: 1.8;",
          txt)))})
  
  
  output$IVPlot <- plotly::renderPlotly({
    shiny::validate(
      shiny::need(
        !base::is.null(input$IVPark) &&
          base::nzchar(input$IVPark) &&
          !(base::identical(input$IVSpeciesType, "Pick") &&
              (base::is.null(input$IVSpecies) ||
                 base::length(input$IVSpecies) == 0)),
        "There are no results for this combination of choices. Please select a park, species, or plant type."
      )
    )
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
 
  # footnote
  output$IVLimitWarning <- shiny::renderUI({
    shiny::req(
      !base::is.null(input$IVPark) && base::nzchar(input$IVPark),
      !(base::identical(input$IVSpeciesType, "Pick") &&
          (base::is.null(input$IVSpecies) || base::length(input$IVSpecies) == 0)))
    df <- base::tryCatch(IVData(), error = function(e) NULL)
    shiny::req(!base::is.null(df), base::nrow(df) > 0)
    htmltools::tags$div(
      style = "font-size: 12px; color: #888; font-style: italic; margin-top: 6px; text-align: center;",
      "* Note: Not all species names may be shown on the plot above. See the summary report or data table for the full list.")})
                    
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
  #    "There are no results for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."))
  #  df <- IVData()
  #  shiny::validate(shiny::need(!base::is.null(df) && base::nrow(df) > 0,
  #    "There are no results for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years."))
  #  df %>% dplyr::select(-LabelOpp)})
  
  
  
  tempIVTable <- shiny::reactive({
    shiny::validate(shiny::need(
      !base::is.null(input$IVPark) && base::nzchar(input$IVPark),
      "There are no results for this combination of choices. Please select a park, species, or plant type."))
    
    df <- IVData()
    
    shiny::validate(shiny::need(
      !base::is.null(df) && base::nrow(df) > 0,
      "There are no results for this combination of choices. Please select a park, species, or plant type."))
    df <- base::switch(input$IVSpeciesType,
                       Common = {
                         shiny::req(input$IVTop)
                         df %>%
                           dplyr::slice_max(order_by = Total, n = input$IVTop, with_ties = FALSE) %>%
                           dplyr::arrange(dplyr::desc(Total))},
                       Least = {
                         shiny::req(input$IVTop)
                         df %>%
                           dplyr::slice_min(order_by = Total, n = input$IVTop, with_ties = FALSE) %>%
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


  output$IVData <- DT::renderDataTable({
    shiny::req(!(input$IVSpeciesType == "Pick" && (base::is.null(input$IVSpecies) || base::length(input$IVSpecies) == 0)))
    
    DT::datatable(tempIVTable(), options = base::list(dom = "t", pageLength = -1, ordering = FALSE, 
                                                      columnDefs = base::list(base::list(className = 'dt-left', targets = "_all")))) %>%
      DT::formatRound(columns = base::intersect(base::c("Density", "Size", "Distribution", "Total"), base::names(tempIVTable())), digits = 2)})


  
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
  
  output$hasSpPark <- shiny::reactive({!base::is.null(input$SpListPark) && input$SpListPark != ""})
  shiny::outputOptions(output, "hasSpPark", suspendWhenHidden = FALSE)
  
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
    shiny::validate(shiny::need(input$SpListPark != "",""))
    df <- SpeciesTableData()
    shiny::validate(shiny::need(base::is.data.frame(df), "Data not available."))
    DT::datatable(df, rownames = FALSE, selection = "single", class = "display compact") |>
      DT::formatStyle("Latin Name", fontStyle = "italic")})
  
  output$NPSpeciesLink <- shiny::renderUI({
    shiny::req(input$SpListPark)
    if (input$SpListType != "NPSpecies") {base::return(NULL)}
    
    link_info <- switch(input$SpListPark, 
      "ANTI" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/ANTI",
        label = "View full Antietam species list"),
      "CATO" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/CATO",
        label = "View full Catoctin species list"),
      "CHOH" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/CHOH",
        label = "View full C&O Canal species list"),
      "GWMP" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/GWMP",
        label = "View full GW Parkway species list"),
      "HAFE" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/HAFE",
        label = "View full Harpers Ferry species list"),
      "MANA" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/MANA",
        label = "View full Manassas species list"),
      "MONO" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/MONO",
        label = "View full Monocacy species list"),
      "NACE" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/NACE",
        label = "View full National Capital Parks – East species list"),
      "PRWI" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/PRWI",
        label = "View full Prince William species list"),
      "ROCR" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/ROCR",
        label = "View full Rock Creek species list"),
      "WOTR" = base::list(url = "https://irma.nps.gov/NPSpecies/Search/SpeciesList/WOTR",
        label = "View full Wolf Trap species list"),
      
      base::list(url = base::paste0("https://irma.nps.gov/NPSpecies/Search/SpeciesList/",
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
