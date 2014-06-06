library(shiny)
library(NPSForVeg)
library(leaflet)
library(lattice)


NCRN<-importNCRN("T:/I&M/MONITORING/Forest_Vegetation/RCode/VegData")
names(NCRN)<-getNames(NCRN, name.class="code")
ParkList<-getNames(NCRN,name.class="code")
names(ParkList)<-getNames(NCRN)

shinyServer(function(input, output,session){

  ######## main Map   
  map<-createLeafletMap(session,"map")
  
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
    map$clearShapes()
    # Draw in batches of 1000; makes the app feel a bit more responsive
    #chunksize <- 1000
    #for (from in seq.int(1, nrow(zipdata), chunksize)) {
    #  to <- min(nrow(zipdata), from + chunksize)
    #  zipchunk <- zipdata[from:to,]
      # Bug in Shiny causes this to error out when user closes browser
      # before we get here
  #    try(
        map$addCircle(getPlots(NCRN)$Latitude,getPlots(NCRN)$Longitude, 15, options=list(color="red",fillOpacity=.5)
        #  zipchunk$latitude, zipchunk$longitude,
        #  (zipchunk[[sizeBy]] / max(allzips[[sizeBy]])) * 30000,
        #  zipchunk$zipcode,
        #  list(stroke=FALSE, fill=TRUE, fillOpacity=0.4),
        #  list(color = colors[from:to])
        )
   #   )
    #})
  })
  
  # TIL this is necessary in order to prevent the observer from
  # attempting to write to the websocket after the session is gone.
 # session$onSessionEnded(paintObs$suspend)
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
