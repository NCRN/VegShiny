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
  
############ Park Control for Density plot  
  output$ParkControl<-renderUI({
    selectizeInput(inputId="ParkIn",choices=ParkList, label="Park:",
                   options = list(placeholder='Choose a park',
                           onInitialize = I('function() { this.setValue(""); }') )) 
    })



############ Plant select Input for Map#####
#output$MpGrp<-renderText({class(input$MapGroup)})

MapSp<-reactive({sort(unique(getPlants(object=NCRN,group="trees", years=c(2010:2013))$Latin_Name)) 
                 })


output$SpeciesControl<-renderUI({
  selectInput(inputId="MapSpecies", label="Choose a species", selectize=F, choices=MapSp())
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

PlantVals<-reactive({SiteXSpec(object=NCRN,group="trees",years=c(2010:2013), species=input$MapSpecies, Total=F)[[2]]})

############ Add points to map
session$onFlushed(once=TRUE, function() {   ##onFlushed comes superzip - makes map draw befrore circles
 # paintObs <- observe({
    #colorBy <- input$color
    #sizeBy <- input$size
    
    #colorData <- if (colorBy == "superzip") {
    #  as.numeric(allzips$centile > (100 - input$threshold))
    #} else {
    #  allzips[[colorBy]]
    #}
    #colors <- brewer.pal(7, "Spectral")[cut(colorData, 7, labels = FALSE)]
    #colors <- colors[match(zipdata$zipcode, allzips$zipcode)]
    
    # Clear existing circles before drawing
  MapObs<-observe({ 
  map$clearShapes()

      # Bug in Shiny causes this to error out when user closes browser
      # before we get here
  #    try(
       #map$addCircle(getPlots(NCRN)$Latitude,getPlots(NCRN)$Longitude, 10*PlantVals(), options=list(color="red",fillOpacity=.5))

  map$addCircle(getPlots(NCRN)$Latitude,getPlots(NCRN)$Longitude, 15+PlantVals(), options=list(color= brewer.pal(7, "Spectral")[cut(PlantVals(), 7, labels = FALSE)],fillOpacity=.75))

        #  zipchunk$latitude, zipchunk$longitude,
        #  (zipchunk[[sizeBy]] / max(allzips[[sizeBy]])) * 30000,
        #  zipchunk$zipcode,
        #  list(stroke=FALSE, fill=TRUE, fillOpacity=0.4),
        #  list(color = colors[from:to])
         })
   #   )
    #})
  
  })
  
  # TIL this is necessary in order to prevent the observer from
  # attempting to write to the websocket after the session is gone.
#session$onSessionEnded(MapObs$suspend)
})

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
