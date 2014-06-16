library(shiny)
library(NPSForVeg)
library(leaflet)
library(lattice)


NCRN<-importNCRN("T:/I&M/MONITORING/Forest_Vegetation/RCode/VegData")
names(NCRN)<-getNames(NCRN, name.class="code")
ParkList<-getNames(NCRN,name.class="code")
names(ParkList)<-getNames(NCRN)

ParkBounds<-read.csv("boundboxes.csv", as.is=TRUE)



shinyServer(function(input,output,session){

################################## Code For Map Panel  ######################################################

### Create Map  
map<-createLeafletMap(session,"map")

#######  Park Filter for species list control for map

output$MapParkControl<-renderUI({
  selectInput(inputId="MapPark", label="Filter species list by park",
              choices=c(All="All",ParkList) )
})

########### Species list control for map 
output$MapSpeciesControl<-renderUI({
  if(is.null(input$MapPark) || nchar(input$MapPark)==0) {
    return()
  }
  else{
    if(input$MapPark=="All"){
    selectizeInput(inputId="MapSpecies", label="Choose a species", 
             choices=c("All Species"="All",sort(unique(getPlants(object=NCRN, group=input$MapGroup, years=c(2010:2013))$Latin_Name))) )
    }
    else{
      selectInput(inputId="MapSpecies", label="Choose a species", 
                  choices=c("All Species"="All",sort(unique(getPlants(object=NCRN[input$MapPark], group=input$MapGroup, 
                                          years=c(2010:2013))$Latin_Name ))) )
    }
  }
})

#### Data to display control for Map
ValuesUse<-reactive({
  switch(input$MapGroup,
         trees=,saplings=c(Abundance="count", "Basal Area"="size"),
         seedlings=,shseedlings=,shrubs=,vines=c(Abundance="count"),
         herbs=c("Percent Cover"="size")
         
    )
})

output$PlantValueControl<-renderUI({
  selectInput(inputId="MapValues", label="Data to Map:", choices=ValuesUse())
  
})

######## Zoom control for map

output$ParkZoomControl<-renderUI({
  selectInput(inputId="ParkZoom",label=NULL,
              choices=c(All="All",ParkList) )
})

###   Calculate the values for the circles on the map.
MapSpeciesType<-reactive({ifelse(input$MapSpecies=="All", "Total","Individual") })
MapSpeciesUse<-reactive({ifelse(input$MapSpecies=="All", NA, input$MapSpecies) })

#PlantVals<-reactive({
#  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){
#    return()
#  }
#  else{
#    if(input$MapGroup != "herbs"){
#      (10000/getArea(NCRN[1],group=input$MapGroup,type="all")) * (
#      SiteXSpec(object=NCRN,group=input$MapGroup, years=c(2010:2013), species=MapSpeciesUse(), values=input$MapValues)$Total)
#    }
#    else{
#      SiteXSpec(object=NCRN,group=input$MapGroup, years=c(2010:2013), species=MapSpeciesUse(), values=input$MapValues)$Total/12
#    }
#  }
#})

MapData<-reactive({
  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){
    return()
  }
  else{
    data.frame(getPlots(NCRN, years=c(2010:2013), output="dataframe")[c("Plot_Name","Unit_Code","Latitude","Longitude")],
      Values=  
        if(input$MapGroup != "herbs"){
          (10000/getArea(NCRN[1],group=input$MapGroup,type="all")) * (
          SiteXSpec(object=NCRN,group=input$MapGroup, years=c(2010:2013), species=MapSpeciesUse(), values=input$MapValues)$Total)
        }
        else{
        SiteXSpec(object=NCRN,group=input$MapGroup, years=c(2010:2013), species=MapSpeciesUse(), values=input$MapValues)$Total/12
        } 
    )
  }
})

############ Add points to map
#MapObs<-return()
session$onFlushed(once=TRUE, function() {   ##onFlushed comes superzip - makes map draw befrore circles
  MapObs<-observe({ 

    map$clearShapes()
    try(                               #try deals with issue where the group has changed but species has not yet caught up.
      if(is.null(MapData()$Values )) {
        return()
    }
    else {
      map$addCircle(as.character(MapData()$Latitude), as.character(MapData()$Longitude), 15, 
         options=list(color=BluePur(5)[cut(MapData()$Values,
          breaks=c(MapLegend[[MapSpeciesType()]][[input$MapValues]][[input$MapGroup]]$Cuts), 
          labels = FALSE)],fillOpacity=.7, weight=5) )
    }
  )
})
  
  
  # TIL this is necessary in order to prevent the observer from
  # attempting to write to the websocket after the session is gone.
session$onSessionEnded(MapObs$suspend)
})


############Zoom the map
observe({
  input$MapZoom
  isolate({
    BoundsUse<-reactive({ as.numeric(ParkBounds[ParkBounds$ParkCode==input$ParkZoom,2:5]) })
    map$fitBounds(BoundsUse()[1],BoundsUse()[2],BoundsUse()[3],BoundsUse()[4])
  }) 
})
######################  UIoutput for Legend
output$MapLegend<-renderUI({
  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){
    return()
  }
  else{
    tags$table(
    mapply(
      function(BoxLabel,color){
        tags$tr(tags$td(tags$div(
          style=sprintf("width: 16px; height: 16px; background-color: %s;", color)
        )),
        tags$td(": ",BoxLabel)
        )}, 
      c(MapLegend[[MapSpeciesType()]][[input$MapValues]][[input$MapGroup]]$Labels),BluePur(5),SIMPLIFY=FALSE ))
  }  
})

output$MapLegendTitle<-renderUI({ 
  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){
    return()
  }
  else{
    h4(MapLegend[[MapSpeciesType()]][[input$MapValues]][[input$MapGroup]]$Title) 
  }
})



#Show a popup at the given location
showPlotPopup <- function(Plot_Name, lat, lng) {
  selectedPlot <- MapData()[MapData()$Plot_Name == Plot_Name,]
  content <- as.character(tagList(
   tags$h4("Park", selectedPlot$Unit_Code)
#    tags$strong(HTML(sprintf("%s, %s %s",
#                             selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
#    ))), tags$br(),
#    sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
#    sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
#    sprintf("Adult population: %s", selectedZip$adultpop)
  ))
  map$showPopup(lat, lng, Plot_Name)
}

# When map is clicked, show a popup with city info
clickObs <- observe({
  map$clearPopups()
  event <- input$map_shape_click
  if (is.null(event))
    return()
  
  isolate({
    showPlotPopup(event$id, event$lat, event$lng)
  })
})


############ Park Control for Density plot  
output$ParkControl<-renderUI({
  selectizeInput(inputId="ParkIn",choices=ParkList, label="Park:",
                 options = list(placeholder='Choose a park',
                                onInitialize = I('function() { this.setValue(""); }') )) 
})



############Density Plot Function
output$Testdens<-renderPlot({
  print(
    if (is.null(input$ParkIn) || nchar(input$ParkIn)==0) {return()}
    else{
      densplot(NCRN[input$ParkIn], densargs=list(group=input$densgroup, years=c((input$YearIn-3):input$YearIn),values=input$densvalues),
               top=input$TopIn, Total=F)
    }
  )
})




})


