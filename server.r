library(shiny)
library(NPSForVeg)
library(leaflet)
library(lattice)
library(rgdal)

#################### Temp storage for function to create polygons
GetPolys<-function(MapIn){
  lat<-lng<-MapClass<-NULL
  for(i in seq_along (MapIn@polygons)){
    #for(j in seq_along(Map@polygons[[i]]@Polygons)){
        lng<-c(lng,MapIn@polygons[[i]]@Polygons[[1]]@coords[,1],NA)
        lat<-c(lat,MapIn@polygons[[i]]@Polygons[[1]]@coords[,2],NA)
        MapClass<-c(MapClass,if(!is.na(MapIn@data[i,"MapClass"])){as.character(MapIn@data[i,"MapClass"])} else("Not classified")  )
    #}
  }
 ClassNum<-as.numeric(factor(MapClass))
 layerId<-as.character(seq_along(MapClass))
  MapData<-list(lat=lat, lng=lng, MapClass=MapClass,layerId=layerId, ClassNum=ClassNum)
}
##################### Housekeeping prior to start of the server funciton

NCRN<-importNCRN("T:/I&M/MONITORING/Forest_Vegetation/RCode/VegData")
names(NCRN)<-getNames(NCRN, name.class="code")
ParkList<-getNames(NCRN,name.class="code")
names(ParkList)<-getNames(NCRN)

ParkBounds<-read.csv("boundboxes.csv", as.is=TRUE)



#################### Begin Server Function

shinyServer(function(input,output,session){

################################## Code For Map Panel  ######################################################

### Create Map  
map<-createLeafletMap(session,"map")

MapYears<-reactive({  # (as.numeric(input$MapYear)-3):as.numeric(input$MapYear)  }) 
  # c(2010:2013)}) #may be replaced by a slider
  (input$MapYear-3):input$MapYear  })
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
             choices=c("All Species"="All",sort(unique(getPlants(object=NCRN, group=input$MapGroup, years=MapYears(), 
                                                                  )$Latin_Name))) )
    }
    else{
      selectInput(inputId="MapSpecies", label="Choose a species", 
                  choices=c("All Species"="All",sort(unique(getPlants(object=NCRN[input$MapPark], group=input$MapGroup, 
                                          years=MapYears() )$Latin_Name ))) )
    }
  }
})


####################   Calculate the values for the circles on the map. ######################

### HouseKeeping

MapSpeciesUse<-reactive({ifelse(input$MapSpecies=="All", NA, input$MapSpecies) })


MapData<-reactive({
  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){
    return()
  }
  else{
    data.frame(getPlots(NCRN, years=MapYears(), output="dataframe",type="all")[c("Plot_Name","Unit_Code","Latitude","Longitude")],
      Year=getEvents(object=NCRN, years=MapYears())[["Event_Year"]],
      Values= 
        if(input$MapGroup != "herbs"){
          if(input$MapValues!="size"){
            (10000/getArea(NCRN[1],group=input$MapGroup,type="all")) * (
            SiteXSpec(object=NCRN,group=input$MapGroup, years=MapYears(), species=MapSpeciesUse(), values=input$MapValues)$Total)
          }
          else{
            (10000/getArea(NCRN[1],group=input$MapGroup,type="all")) * (    #need to convert to m^2/ha from cm^2/ha
              SiteXSpec(object=NCRN,group=input$MapGroup, years=MapYears(), species=MapSpeciesUse(), values=input$MapValues)$Total)/10000
          }
        }
        else{
        SiteXSpec(object=NCRN,group=input$MapGroup, years=MapYears(), species=MapSpeciesUse(), values=input$MapValues)$Total/12
        } 
    )
  }
})
#########  Data for Legend
MapMetaData<-reactive({ MapLegend[[input$MapValues]][[input$MapGroup]] })


#### Get polygons to display
MapLayer<-reactive({
  switch(input$MapLayer,
  None=return(),
  ForArea=GetPolys(readOGR("./Maps",layer="Forest_NLCD_2011_Clip_WGS84_Simplified")),
  #SoilMap=GetPolys(readOGR("./Maps",layer="SOIL_TaxonomySSURGO_NCRN_py_WGS84_Dissolved_SinglePart"))
  SoilMap=GetPolys(readOGR("./Maps",layer="SOIL_TaxonomySSURGO_NCRN_py_WGS84_SinglePart_Dissolved"))
  )
})



############ Add points and Polygons to map
PolyOpts<-reactive({
  lapply(X=MapLayer()$ClassNum, Y=length(unique(MapLayer()$MapClass)), FUN=function(X,Y) {
  list(color=AquaYel(Y)[X])
  })
})

session$onFlushed(once=TRUE, function() {   ##onFlushed comes superzip - makes map draw befrore circles
  MapObs<-observe({ 
    input$ShowMap
   map$clearShapes()

if(input$MapLayer!="none"){   
  map$addPolygon(lng=MapLayer()$lng, 
                 lat=MapLayer()$lat,  
                 layerId=MapLayer()$layerId, 
                 options=PolyOpts(),
                 defaultOptions=list(weight=0, fillOpacity=.75)
  )
}
 
 try(silent=TRUE,                       #try deals with issue where the group has changed but species has not yet caught up.
      if(is.null(MapData()$Values )) {
        return()
      }
      else {
        map$addCircle(MapData()$Latitude, MapData()$Longitude, 15*as.numeric(input$PlotSize),
          layerId=MapData()$Plot_Name,   #This is the ID of the circle to match to other data
          options=list(color=BlueOr(8)[cut(MapData()$Values,breaks=c(MapMetaData()$Cuts), labels = FALSE)],
          fillOpacity=.7, 
          weight=5)
        )
      }
    ) 
  })
  
  # TIL this is necessary in order to prevent the observer from
  # attempting to write to the websocket after the session is gone.
session$onSessionEnded(MapObs$suspend)
})



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
      c(MapMetaData()$Labels),BlueOr(8),SIMPLIFY=FALSE ))
  }
 
})


output$MapLegendTitle<-renderText({ 
  if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){
    return()
  }
  else{
    MapMetaData()$Title
  }
})



### Function for adding information to, and displaying, popup.
showPlotPopup <- function(Plot_Name, lat, lng) {
  selectedPlot <- MapData()[MapData()$Plot_Name == Plot_Name,]
  content <- as.character(tagList(
   tags$h5(getNames(NCRN[selectedPlot$Unit_Code],"long")),
   tags$h6("Year Monitored:",selectedPlot$Year),
   tags$h6(input$MapSpecies,":",format(signif(selectedPlot$Values,2), big.mark=","), " ", MapMetaData()$Title)
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

############## Legend for Map Layers
output$LayerLegendTitle<-renderText({
  switch(input$MapLayer,
         None=return(),
         ForArea=return("Forested Area"),
         SoilMap=return("Soil Type")
         
  )
})


output$LayerLegend<-renderUI({
  
  if(input$MapLayer=="None"){
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
        c(sort(unique(MapLayer()$MapClass))), AquaYel( length( unique(MapLayer()$MapClass))), SIMPLIFY=FALSE ))
  }
  
})


############################## Plots Tab ###############################################################################

############ Park Control for Density plot  
output$densParkControl<-renderUI({
  selectizeInput(inputId="densPark",choices=ParkList, label="Park:",
                 options = list(placeholder='Choose a park',
                                onInitialize = I('function() { this.setValue(""); }') )) 
})

############### Years to plot from control
densYears<-reactive({ (input$densYear-3):input$densYear })


##################### Data to display control for density plot
DensValuesUse<-reactive({
  switch(input$densGroup,
         trees=,saplings=c(Abundance="count", "Basal Area"="size", "Proportion of Plots Occupied"="presab"),
         seedlings=,shseedlings=,shrubs=,vines=c(Abundance="count","Proportion of Plots Occupied"="presab"),
         herbs=c("Percent Cover"="size","Proportion of Plots Occupied"="presab")
         
  )
})

output$densValControl<-renderUI({
  selectInput(inputId="densvalues", label="Data to Graph:", choices=DensValuesUse())
  
})
############################ Species Control (top species vs list) for density plots

output$densSpeciesControl<-renderUI({
  switch(input$densSpeciesType,
         Common= tags$div(title="# of species to display", 
            sliderInput(inputId="densTop",label="Number of species to display (in order of mean value):",
                min=1, max=10,value=5, format="##")
          ),
         Pick= if(is.null(input$densPark) || nchar(input$densPark)==0) {  return()  }
          else{
            tags$div(title="Click here to pick the species you want to graph",
              selectizeInput(inputId="densSpecies", label="Choose one or more species (Latin name only),
              backspace to remove", choices=c(sort(unique(getPlants(object=NCRN[input$densPark], group=input$densGroup,
                years=densYears())$Latin_Name ))),multiple=TRUE )
            )
          }
  )
})


####################### Control for comparison


output$CompareSelect<-renderUI({
  switch(input$CompareType, 
    None=,return(),
    Park= tags$div(title= "Choose a second park",
            selectizeInput(inputId="ComparePark",choices=ParkList, label="Park:",
           options = list(placeholder='Choose a park',onInitialize = I('function() { this.setValue(""); }') ))
          ),
    
    "Growth Stage"=tags$div(title="Choose an additional growth stage",
                    selectizeInput(inputId="CompareGroup", label="Growth Stage:", 
                      choices=switch(input$densGroup,
                      trees=,saplings=,seedlings=c(Trees="trees", Saplings="saplings", "Tree Seedlings"="seedlings"),
                      shrubs=, shseedlings=c(Shrubs="shrubs", "Shrub Seedlings"="shseedlings"),
                      vines=,herbs=c('Only one growth stage monitored.'=NA)
                      )
                    )
                  ),
    Time=tags$div(title= "Choose a second range of years",
                sliderInput(inputId="CompareYear", label="Display data from the 4 years ending:", min=2009, max=2013, 
                            value=2013, format="####"))
  )
})
############## Need Compare species to keep the number of species to display to accepted number
CompareSpecies<-reactive({
  if(input$CompareType=="None") {NA} else {
    switch(input$densSpeciesType,
          Common=as.character( dens(object=NCRN[input$densPark], group=input$densGroup, years=densYears(),values=input$densvalues,
            Total=F)[order(-dens(object=NCRN[input$densPark], group=input$densGroup, years=densYears(), values=input$densvalues, 
            Total=F)["Mean"]),][1:input$densTop,1] ),
          Pick=input$densSpecies,
          All=NA
    )
  }
})


################### make compare and labels arguments for densplot()

DensCompare<-reactive({switch(input$CompareType,
    None=return(NA),
    Park=  if (is.null(input$ComparePark) || nchar(input$ComparePark)==0) {return(NA)}
      else{
        return(list(object=NCRN[input$ComparePark], group=input$densGroup,  years=densYears(),
                    values=input$densvalues, species=CompareSpecies(), common=input$densCommon ))
      },
    "Growth Stage"=return(list(object=NCRN[input$densPark], group=input$CompareGroup, years=densYears(),
                    values=input$densvalues, species=CompareSpecies(),common=input$densCommon )),
    Time=return(list(object=NCRN[input$densPark], group=input$densGroup, years=c((input$CompareYear-3):input$CompareYear),
                     values=input$densvalues, species=CompareSpecies(),common=input$densCommon))
    )
})

DensLabelData<-data.frame(Name=c("trees","saplings","seedlings","shrubs","shseedlings","herbs","vines"), Label=c("Trees","Saplings","Tree Seedlings", "Shrubs","Shrub Seedlings","Understory Plants","Vines in Trees"), stringsAsFactors=FALSE)

DensLabels<-reactive({switch(input$CompareType,
    None=return(NA),
    Park=  if (is.null(input$ComparePark) || nchar(input$ComparePark)==0) {return(NA)}
            else{return(c(getNames(object=NCRN[input$densPark],"short"), getNames(object=NCRN[input$ComparePark], "short") ) )},
    "Growth Stage"=if (is.null(input$CompareGroup) || nchar(input$CompareGroup)==0) {return(NA)}
            else{ return(c(DensLabelData[DensLabelData$Name==input$densGroup,]$Label,
                           DensLabelData[DensLabelData$Name==input$CompareGroup,]$Label))},
    Time=if (is.null(input$CompareYear) || nchar(input$CompareYear)==0) {return(NA)}
              else{return(c( paste0(as.character(input$densYear-3),"-",as.character(input$densYear)),
                  paste0(as.character(input$CompareYear-3),"-",as.character(input$CompareYear)) ))}
  ) 
})

############### Y axis labels for density plot
densYlabel<-reactive({
  switch(input$densvalues,
    count=switch(input$densGroup,
      trees="Trees / ha",
      saplings="Saplings / ha",
      seedlings="Tree seedlings / ha",
      shrubs="Shrubs / ha",
      shseedlings="Shrub seedlings / ha",
      vines="Vines on Trees / ha",
    ),
    size=switch(input$densGroup,
      trees=,saplings="Basal area cm2/ ha",
      herbs="Percent Cover"
    ),
    presab="Proportion of Plots Occupied"
  )
})

############### Title for density plot
densTitleGroup<-reactive({
  switch(input$densGroup,
         trees="Tree",
         saplings="Sapling",
         seedlings="Tree Seedling",
         shrubs="Shrub",
         shseedlings="Shrub Seedlings",
         herbs="Understory Plant",
         vines="Vines on Trees"
         )  
})
compareTitleGroup<-reactive({
  switch(input$CompareGroup,
         trees="Tree",
         saplings="Sapling",
         seedlings="Tree Seedling",
         shrubs="Shrub",
         shseedlings="Shrub Seedlings",
         herbs="Understory Plant",
         vines="Vines on Trees"
  )  
})

densTitleValues<-reactive({
  switch(input$densvalues,
         count="Abundance",
         size=switch(input$densGroup,
                     trees=,saplings="Basal Area",
                     herbs="Percent Cover"
         ),
         presab="Proportion of Plots Occupied"
  )
})
DensTitle<-reactive({
  switch(input$CompareType,
         None=  return(paste(getNames(NCRN[input$densPark],"long"),":",densTitleGroup(),densTitleValues(), 
               paste0(as.character(input$densYear-3),"-",as.character(input$densYear)) )),
         Park=return(paste(getNames(NCRN[input$densPark],"long"),"vs.",getNames(NCRN[input$ComparePark],"long"),":",
                           densTitleGroup(),densTitleValues(), 
                           paste0(as.character(input$densYear-3),"-",as.character(input$densYear)) )),
         "Growth Stage"= return(paste(getNames(NCRN[input$densPark],"long"),":",densTitleGroup(),"vs.",
                                      compareTitleGroup(), densTitleValues(), 
                                      paste0(as.character(input$densYear-3),"-",as.character(input$densYear)) )),
         Time=return(paste(getNames(NCRN[input$densPark],"long"),":",densTitleGroup(),densTitleValues(), 
                           paste0(as.character(input$densYear-3),"-",as.character(input$densYear)),"vs.",
                           paste0(as.character(input$CompareYear-3),"-",as.character(input$CompareYear)) ))
  )
})

################ All arguments for densityPlot
DensPlotArgs<-reactive({
  list(
    object=NCRN[input$densPark],
    densargs=list(
      group=input$densGroup,
      years=densYears(),
      values=input$densvalues,
      common=input$densCommon,
      species=switch(input$densSpeciesType,
        Pick= {species=input$densSpecies},
        Common=  {species=NA},
        All= {species=NA}
      )
    ),
    compare=list(DensCompare()),
    labels=DensLabels(),
    top=switch(input$densSpeciesType,
                Common={top=input$densTop},
                Pick={top=NA},
                All = {top=0}
    ),
    Total=if(input$densSpeciesType=="All"){Total=T} else {Total=F} ,
    col=rainbow(10),
    ylab=densYlabel(),
    main=DensTitle()
  )
})


############Density Plot Function
output$DensPlot<-renderPlot({
    if (is.null(input$densPark) || nchar(input$densPark)==0) {return()}
    else{
      validate(need(try(
        do.call(densplot,DensPlotArgs() )),
       "There is no data for this combination of choices. The type of plant you selected was not found in the park during those years."
       ))
      do.call(densplot, DensPlotArgs())
    }
})

DensTableArgs<-reactive({
  list(
    object=DensPlotArgs()$object,
    group=DensPlotArgs()$densargs$group,
    years=DensPlotArgs()$densargs$years,
    values=DensPlotArgs()$densargs$values,
    common=DensPlotArgs()$densargs$common
  )
})

output$densTable<-renderDataTable({
  validate(need(try(
    do.call(dens, DensTableArgs() )),
    "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years"
           ))
  TableOut<-do.call(dens,DensTableArgs())
  names(TableOut)<-c("Species",'Mean',"Lower 95% CI", "Upper 95% CI")
  return(TableOut)
})



########################################################## IV Plots
############ Park Control for IV plot  
output$IVParkControl<-renderUI({
  selectizeInput(inputId="IVPark",choices=ParkList, label="Park:",
                 options = list(placeholder='Choose a park',
                                onInitialize = I('function() { this.setValue(""); }') )) 
})



################ All arguments for IVPlot
IVYears<-reactive({ (input$IVYear-3):input$IVYear})

##Title for Iv plot
IVTitleGroup<-reactive({
  switch(input$IVGroup,
         trees="Tree",
         saplings="Sapling",
         seedlings="Tree Seedling",
         shseedlings="Shrub Seedlings",
         herbs="Understory Plant"
  )
})

         
IVTitle<-reactive({
  return(paste(getNames(NCRN[input$IVPark],"long"),":","\n", IVTitleGroup(),"Importance Values", 
                             paste0(as.character(input$IVYear-3),"-",as.character(input$IVYear)) ))
         
})

IVPlotArgs<-reactive({
  list(
    object=NCRN[input$IVPark],
    IVargs=list(
      group=input$IVGroup,
      years=IVYears(),
      common=input$IVCommon
    ),
    parts=input$IVPart,
    top=input$IVTop,
    compare=NA,
    labels=NA,
    main=IVTitle()
  )
})

output$IVPlot<-renderPlot({
 if (is.null(input$IVPark) || nchar(input$IVPark)==0) {validate(need(input$IVPark, "Please select a park"))}
 else{ 
  validate(need(try(
      do.call(IVplot,IVPlotArgs() )),
      "There is no data for this combination of choices. The type of plant you selected was not found in the park during those years."
    ))
    update(do.call(IVplot, IVPlotArgs()), scales=list(cex=1.04))
  }
})

IVTableArgs<-reactive({
  list(
    object=IVPlotArgs()$object,
    group=IVPlotArgs()$IVargs$group, 
    years=IVPlotArgs()$IVargs$years, 
    common=IVPlotArgs()$IVargs$common
  )
})
    
    
output$IVData<-renderDataTable({
  validate(need(try(
    do.call(IV,IVTableArgs() )),
    "Please select a park"
  ))
  do.call(IV,IVTableArgs())
})
  



})# end of shinyServer() function


