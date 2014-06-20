library(shiny)
library(NPSForVeg)
library(leaflet)
library(lattice)
library(maptools)


##################### Housekeeping prior to start of the server funciton

NCRN<-importNCRN("T:/I&M/MONITORING/Forest_Vegetation/RCode/VegData")
names(NCRN)<-getNames(NCRN, name.class="code")
ParkList<-getNames(NCRN,name.class="code")
names(ParkList)<-getNames(NCRN)

ParkBounds<-read.csv("boundboxes.csv", as.is=TRUE)
#PRWIMap<-readShapePoly("./Maps/VEG_Veg_Systems_py")




#################### Begin Server Function

shinyServer(function(input,output,session){

################################## Code For Map Panel  ######################################################

### Create Map  
map<-createLeafletMap(session,"map")

MapYears<-c(2010:2013)  #may be replaced by a slider

#########################################################################################################

######## Zoom control for map

output$ParkZoomControl<-renderUI({
  selectInput(inputId="ParkZoom",label=NULL,
              choices=c(All="All",ParkList) )
})


############Zoom the map
observe({
  input$MapZoom
  isolate({
    BoundsUse<-reactive({ as.numeric(ParkBounds[ParkBounds$ParkCode==input$ParkZoom,2:5]) })
    map$fitBounds(BoundsUse()[1],BoundsUse()[2],BoundsUse()[3],BoundsUse()[4])
  }) 
})

###########################################################################################################

#######  Park Filter for species list control for map

output$MapParkControl<-renderUI({
  selectInput(inputId="MapPark", label="Filter species list by park",
              choices=c(All="All",ParkList) )
})

############################## Data to map control################################################

#### Data to display control for Map
ValuesUse<-reactive({
  switch(input$MapGroup,
         trees=,saplings=c(Abundance="count", "Basal Area"="size"),
         seedlings=,shseedlings=,shrubs=,vines=c(Abundance="count"),
         herbs=c("Percent Cover"="size")
         
  )
})

#render the control
output$PlantValueControl<-renderUI({
  selectInput(inputId="MapValues", label="Data to Map:", choices=ValuesUse())
  
})

#################################################################################################

########### Species list control for map 
output$MapSpeciesControl<-renderUI({
  if(is.null(input$MapPark) || nchar(input$MapPark)==0) {
    return()
  }
  else{
    if(input$MapPark=="All"){
    selectizeInput(inputId="MapSpecies", label="Choose a species", 
             choices=c("All Species"="All",sort(unique(getPlants(object=NCRN, group=input$MapGroup, years=MapYears)$Latin_Name))) )
    }
    else{
      selectInput(inputId="MapSpecies", label="Choose a species", 
                  choices=c("All Species"="All",sort(unique(getPlants(object=NCRN[input$MapPark], group=input$MapGroup, 
                                          years=MapYears)$Latin_Name ))) )
    }
  }
})

####################   Calculate the values for the circles on the map. ######################

### HouseKeeping

MapSpeciesType<-reactive({ifelse(input$MapSpecies=="All", "Total","Individual") })
MapSpeciesUse<-reactive({ifelse(input$MapSpecies=="All", NA, input$MapSpecies) })


MapData<-reactive({
  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){
    return()
  }
  else{
    data.frame(getPlots(NCRN, years=MapYears, output="dataframe")[c("Plot_Name","Unit_Code","Latitude","Longitude")],
      Values=  
        if(input$MapGroup != "herbs"){
          (10000/getArea(NCRN[1],group=input$MapGroup,type="all")) * (
          SiteXSpec(object=NCRN,group=input$MapGroup, years=MapYears, species=MapSpeciesUse(), values=input$MapValues)$Total)
        }
        else{
        SiteXSpec(object=NCRN,group=input$MapGroup, years=MapYears, species=MapSpeciesUse(), values=input$MapValues)$Total/12
        } 
    )
  }
})
#########  Data for Legend, etc.
MapMetaData<-reactive(MapLegend[[MapSpeciesType()]][[input$MapValues]][[input$MapGroup]])


############ Add points to map

session$onFlushed(once=TRUE, function() {   ##onFlushed comes superzip - makes map draw befrore circles
  MapObs<-observe({ 

    map$clearShapes()
    try(silent=TRUE,                       #try deals with issue where the group has changed but species has not yet caught up.
      if(is.null(MapData()$Values )) {
        return()
      }
      else {
        map$addCircle(as.character(MapData()$Latitude), as.character(MapData()$Longitude), 15*as.numeric(input$PlotSize),
          layerId=MapData()$Plot_Name,   #This is apparently the id of the circle to match to other data
          options=list(color=BluePur(5)[cut(MapData()$Values,
          breaks=c(MapMetaData()$Cuts), 
          labels = FALSE)],fillOpacity=.7, weight=5) )
      }
    ) 
  })
  
  # TIL this is necessary in order to prevent the observer from
  # attempting to write to the websocket after the session is gone.
session$onSessionEnded(MapObs$suspend)
})

#map$addPolygon(PRWIMap)

######################  UIoutput for Legend

output$MapLegend<-renderUI({

  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0 || is.null(MapMetaData()) ){
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
      c(MapMetaData()$Labels),BluePur(5),SIMPLIFY=FALSE ))
  }
 
})


output$MapLegendTitle<-renderUI({ 
  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){
    return()
  }
  else{
    h4(MapMetaData()$Title) 
  }

  })



### Function for adding information to, and displaying, popup.
showPlotPopup <- function(Plot_Name, lat, lng) {
  selectedPlot <- MapData()[MapData()$Plot_Name == Plot_Name,]
  content <- as.character(tagList(
   tags$h5(getNames(NCRN[selectedPlot$Unit_Code],"long")),
   br(),
   tags$h4(input$MapSpecies,":",format(signif(selectedPlot$Values,2), big.mark=","), " ", MapMetaData()$Title)
  ))
  map$showPopup(lat, lng, content, Plot_Name)
}

###  When a plot is clicked, show the popup with plot info
clickObs <- observe({
  map$clearPopups()
  event <- input$map_shape_click
  if (is.null(event))
    return()
  
  isolate({
    showPlotPopup(event$id, as.character(event$lat), as.character(event$lng))
  })
})
session$onSessionEnded(clickObs$suspend)




############################## Plots Tab ###############################################################################


############ Park Control for Density plot  
output$ParkControl<-renderUI({
  selectizeInput(inputId="ParkIn",choices=ParkList, label="Park:",
                 options = list(placeholder='Choose a park',
                                onInitialize = I('function() { this.setValue(""); }') )) 
})



####################### Control for comparison
output$CompareSelect<-renderUI({
  switch(input$CompareType, 
    None=,return(),
    Park= selectizeInput(inputId="ComparePark",choices=ParkList, label="Park:",
           options = list(placeholder='Choose a park',onInitialize = I('function() { this.setValue(""); }') )),
    
    "Growth Stage"=selectizeInput(inputId="CompareGroup", label="Growth Stage:",choices=c(Trees="trees", 
                                          Saplings="saplings", Seedlings="seedlings") ),
    Time=return(sliderInput(inputId="CompareYear", label="Display data from the 4 years ending:", min=2009, max=2013, 
                            value=2013, format="####"))
  )
})
################### make compare and labels arguments for densplot()
DensCompare<-reactive({switch(input$CompareType,
    None=return(NA),
    Park=  if (is.null(input$ComparePark) || nchar(input$ComparePark)==0) {return(NA)}
      else{
        return(list(object=NCRN[input$ComparePark], group=input$densgroup,  years=c((input$YearIn-3):input$YearIn),values=input$densvalues))
      },
    
    "Growth Stage"=return(list(object=NCRN[input$ParkIn], group=input$CompareGroup,years=c((input$YearIn-3):input$YearIn),values=input$densvalues)),
    Time=return(list(object=NCRN[input$ParkIn], group=input$densgroup, years=c((input$CompareYear-3):input$CompareYear),values=input$densvalues))
    )
})

DensLabels<-reactive({switch(input$CompareType,
    None=return(NA),
    Park=  if (is.null(input$ComparePark) || nchar(input$ComparePark)==0) {return(NA)}
    else{return(c(getNames(object=NCRN[input$ParkIn],"short"), getNames(object=NCRN[input$ComparePark], "short") ) )},
    
    "Growth Stage"=if (is.null(input$CompareGroup) || nchar(input$CompareGroup)==0) {return(NA)}
    else{return(c(input$densgroup, input$CompareGroup))},
    
    
    Time=if (is.null(input$CompareYear) || nchar(input$CompareYear)==0) {return(NA)}
    else{return(c( paste0(as.character(input$YearIn-3),"-",as.character(input$YearIn)) ,
                  paste0(as.character(input$CompareYear-3),"-",as.character(input$CompareYear)) ))}
                              
)
})
############Density Plot Function
output$Testdens<-renderPlot({
  print(
    if (is.null(input$ParkIn) || nchar(input$ParkIn)==0) {return()}
    else{
      densplot(NCRN[input$ParkIn], densargs=list(group=input$densgroup, years=c((input$YearIn-3):input$YearIn),values=input$densvalues),
               compare=list(DensCompare()),labels= DensLabels(), top=input$TopIn, Total=F, col=rainbow(10))
    }
  )
})




})


