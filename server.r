library(shiny)
library(NPSForVeg)
library(leaflet)
library(lattice)
library(RColorBrewer)


NCRN<-importNCRN("T:/I&M/MONITORING/Forest_Vegetation/RCode/VegData")
names(NCRN)<-getNames(NCRN, name.class="code")
ParkList<-getNames(NCRN,name.class="code")
names(ParkList)<-getNames(NCRN)

shinyServer(function(input,output,session){

######## main Map   
map<-createLeafletMap(session,"map")

  


############ Plant select Input for Map#####


output$MapParkControl<-renderUI({
  selectInput(inputId="MapPark", label="Filter species list by park",
              choices=c(All="All",ParkList) )
})



output$MapSpeciesControl<-renderUI({
  if(is.null(input$MapPark) || nchar(input$MapPark)==0) {
    return()
}
  else{
    if(input$MapPark=="All"){
    selectInput(inputId="MapSpecies", label="Choose a species", 
             choices=sort(unique(getPlants(object=NCRN, group=input$MapGroup, years=c(2010:2013))$Latin_Name )) )
    }
    else{
      selectInput(inputId="MapSpecies", label="Choose a species", 
                  choices=sort(unique(getPlants(object=NCRN[input$MapPark], group=input$MapGroup, years=c(2010:2013))$Latin_Name )) )
    }
 }
})


#### Park Select for spcies for map
PlantVals<-reactive({
  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){
    return()
  }
  else{
    SiteXSpec(object=NCRN,group=input$MapGroup, years=c(2010:2013), species=input$MapSpecies, Total=F)[[2]]
  }
})

############ Add points to map
session$onFlushed(once=TRUE, function() {   ##onFlushed comes superzip - makes map draw befrore circles

  MapObs<-observe({ 
 
  map$clearShapes()

  if(is.null(PlantVals() )) {
    return()
  }
  else {
    map$addCircle(as.character(getPlots(NCRN)$Latitude), as.character(getPlots(NCRN)$Longitude), 15, options=list(color=brewer.pal(6, "Spectral")[cut(PlantVals(),breaks=c(-1, 0.1, 1.1, 2.1,5.1,10.1,1000), labels = FALSE)],fillOpacity=1, weight=5) )
  }

         })

  })
  
  # TIL this is necessary in order to prevent the observer from
  # attempting to write to the websocket after the session is gone.
#session$onSessionEnded(MapObs$suspend)

# Show a popup at the given location
#showZipcodePopup <- function(zipcode, lat, lng) {
#  selectedZip <- allzips[allzips$zipcode == zipcode,]
#  content <- as.character(tagList(
#    tags$h4("Score:", as.integer(selectedZip$centile)),
#    tags$strong(HTML(sprintf("%s, %s %s",
#                             selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
#    ))), tags$br(),
#    sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
#    sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
#    sprintf("Adult population: %s", selectedZip$adultpop)
#  ))
#  map$showPopup(lat, lng, content, zipcode)
#}
#
#})


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


