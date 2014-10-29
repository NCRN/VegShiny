library(shiny)
library(NPSForVeg)
library(leaflet)
library(lattice)
library(jsonlite,pos=100)
library(httr)

##################### Housekeeping prior to start of the server function


NCRN<-importNCRN("./Data/NCRN")

names(NCRN)<-getNames(NCRN, name.class="code")
ParkList<-getNames(NCRN,name.class="code")
names(ParkList)<-getNames(NCRN)

ParkBounds<-read.csv("boundboxes.csv", as.is=TRUE)

######## geoJson layer with nothing used when "None" is selected for layer
FakeLayer<-c('{"type": "Feature", "geometry": {"type": "Polygon", "coordinates": [[[-77.6, 39.9], [-77.61,39.9], [-77.61,39.91], [-77.6, 39.9]]]}, "properties": {"style": {"fillOpacity": 0.001, "weight": 0}}}') 

#################### Begin Server Function

shinyServer(function(input,output,session){

  Values<-reactiveValues(ShapeMouse=NULL) #reactive values for app
################################## Code For Map Panel  ######################################################

### Create Map  
  map<-createLeafletMap(session,"map")

  MapYears<-reactive({(input$MapYear-3):input$MapYear  })  #put in reactiveValues?
#########################################################################################################

######## Zoom control for map

  output$ParkZoomControl<-renderUI({
    selectInput(inputId="ParkZoom",label=NULL,
              choices=c("All Parks"="All",ParkList) )
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
              choices=c("All Parks"="All",ParkList) )
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
##List of names, elements are Latin names, names of elements are Latin or common
  MapSpecList<-reactive({
    SpecTemp<-unique(getPlants(object=if(input$MapPark=="All") {NCRN}  else {NCRN[[input$MapPark]]} , group=input$MapGroup, 
      years=MapYears(),common=F )$Latin_Name)
    SpecNames<-getPlantNames(object=NCRN[[1]], names=SpecTemp, in.style="Latin",out.style=ifelse(input$mapCommon,"common","Latin"))
    names(SpecTemp)<-SpecNames  
    SpecTemp<-SpecTemp[order(tolower(names(SpecTemp)))]
    SpecTemp<-c("All Species"="All", SpecTemp)
  })

###make the control
  output$MapSpeciesControl<-renderUI({
    if(is.null(input$MapPark) || nchar(input$MapPark)==0) {
      return()
    }
    else{
      selectInput(inputId="MapSpecies", label="Choose a species", choices=c(MapSpecList() ))
    }
  })

####################   Calculate the values for the circles on the map. ######################

### HouseKeeping

  MapSpeciesUse<-reactive({ifelse(input$MapSpecies=="All", NA, input$MapSpecies) }) #put in reactiveValues?

### Data to plot on map - always for all parks
  MapData<-reactive({                                                #change to validate(need())
    if(is.null(input$MapSpecies) || nchar(input$MapSpecies)==0){ 
      return() 
    }
    else{
      data.frame(getPlots(NCRN, years=MapYears(), output="dataframe",type="all")[c("Plot_Name","Unit_Code","Latitude","Longitude")],
        Year=getEvents(object=NCRN, years=MapYears())[["Event_Year"]],
        Values= 
          if(input$MapGroup != "herbs"){
            if(input$MapValues!="size"){
              (10000/getArea(NCRN[[1]],group=input$MapGroup,type="all")) * (
              SiteXSpec(object=NCRN,group=input$MapGroup, years=MapYears(), species=MapSpeciesUse(), values=input$MapValues)$Total)
            }
            else{
              (10000/getArea(NCRN[[1]],group=input$MapGroup,type="all")) * (    #need to convert to m^2/ha from cm^2/ha
                SiteXSpec(object=NCRN,group=input$MapGroup, years=MapYears(), species=MapSpeciesUse(), 
                          values=input$MapValues)$Total)/10000
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



#### Get MapLayer and corresponding data to display


MapLayer<-reactive({
  switch(input$MapLayer,
         None=FakeLayer,
         EcoReg=readChar("./Maps/EcoReg", file.info("./Maps/EcoReg")$size),
         ForArea=readChar("./Maps/Forest", file.info("./Maps/Forest")$size),
         Soil=readChar("./Maps/Soil", file.info("./Maps/Soil")$size)
  )
})

LayerData<-reactive({     
  switch(input$MapLayer,
         None=return(),
         EcoReg=dget("./Maps/EcoRegData.txt"),
         ForArea=dget("./Maps/ForestData.txt"),
         Soil=dget("./Maps/SoilData.txt")
  )
})
  

######### Add points and Polygons to map
 
session$onFlushed(once=TRUE, function() {   ##onFlushed comes superzip - makes map draw befrore circles
  
  #### Add GeoJSON polygon layer
  MapPolys<-observe({ 
    map$addGeoJSON(MapLayer(),layerId="Layer")
  })

  ### add Monitoring plot data as a circle
  MapCircles<-observe({
    input$MapLayer #make sure Circles are always on top
    map$clearShapes()
    try(silent=TRUE,      #try deals with issue where the group has changed but species has not yet caught up.
      if(is.null(MapData()$Values )) {
        return()
      } else {
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
  session$onSessionEnded(MapPolys$suspend)
  session$onSessionEnded(MapCircles$suspend)
})



######################  UIoutput for Cicrle Legend

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
showPlotPopup <- function(PlotId, lat, lng) {
  selectedPlot <- MapData()[MapData()$Plot_Name == PlotId,]
    content<- as.character(tagList(
      tags$h5(getNames(NCRN[[selectedPlot$Unit_Code]],"long")),
      tags$h6("Monitoring Plot:",selectedPlot$Plot_Name),
      tags$h6("Year Monitored:",selectedPlot$Year),
      tags$h6(names(MapSpecList()[MapSpecList()==input$MapSpecies]),":",format(signif(selectedPlot$Values,2), 
                                                                    big.mark=","), " ", MapMetaData()$Title),
      tags$h6("Click on plot to see full list")
    ))
  map$showPopup(lat, lng, content)
  
}
showPlotPopup2 <- function(PlotId, lat, lng) {
 selectedPlot <- MapData()[MapData()$Plot_Name == PlotId,]
 if(
    class(try(SiteXSpec(object=NCRN[[selectedPlot$Unit_Code]], group=input$MapGroup, years=selectedPlot$Year, plots=PlotId, common=input$mapCommon), silent=TRUE))=="try-error") {content<-as.character(tagList(tags$h6("None found on this plot")))} else {
 tempData<-SiteXSpec(object=NCRN[[selectedPlot$Unit_Code]], group=input$MapGroup, years=selectedPlot$Year, plots=PlotId, common=input$mapCommon)
 content<- as.character(tagList(
    tags$h5(getNames(NCRN[[selectedPlot$Unit_Code]],"long")),
    tags$h6("Monitoring Plot:",selectedPlot$Plot_Name),
    tags$h6("Year Monitored:",selectedPlot$Year),
    tags$h6(MapMetaData()$Title),
    tags$table(
      mapply(FUN=function(Name,Value){
        tags$tr(
          tags$td(sprintf("%s: %s", Name, format(signif(Value,2), big.mark=",")))
        )
      },
      names(tempData[-1]), (unlist(tempData[-1])*10000)/getArea(object=NCRN[[selectedPlot$Unit_Code]], group=input$MapGroup,"all"), SIMPLIFY=FALSE
      )
    )
  ))}
 map$showPopup(lat, lng, content)
}


###  When a plot is clicked, show the popup with plot info

MouseOver1<-observe({
  eventOver1<-input$map_shape_mouseover
  if(is.null(eventOver1)){return()}
  isolate(
    showPlotPopup(eventOver1$id, as.character(eventOver1$lat+.001), as.character(eventOver1$lng))
  )
})

MouseOut1<-observe({
  eventOut1<-input$map_shape_mouseout
  map$clearPopups()
  
})

ClickObs1<-observe({
  map$clearPopups()
  eventClick1<-input$map_shape_click
  if(is.null(eventClick1)){return()}
  isolate(
    showPlotPopup2(eventClick1$id, as.character(eventClick1$lat+.001), as.character(eventClick1$lng))
  )
})
###  When a GeoJSON polygon is clicked, show the popup with plot info
ClickObs2<-observe({
  eventClick2<-input$map_geojson_click   #a geojson feature was clicked
  if(is.null(eventClick2)) { return() }
  isolate({
    map$clearPopups()
    map$showPopup(lat=eventClick2$properties$LabelLat,lng=eventClick2$properties$LabelLng, content=eventClick2$properties$MapClass)
  })
  
})

session$onSessionEnded(ClickObs1$suspend)
session$onSessionEnded(ClickObs2$suspend)


############## Legend for Map Layers
output$LayerLegendTitle<-renderText({
  switch(input$MapLayer,
         None=return(),
         ForArea=return("Forested Area"),
         EcoReg=return("Omernik Ecoregions"),
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
        c(sort(unique(as.character(LayerData()$MapClass)))), AquaYel( length( unique(LayerData()$MapClass))), SIMPLIFY=FALSE ))
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
densSpecList<-reactive({
  SpecTemp<-unique(getPlants(object=NCRN[[input$densPark]], group=input$densGroup,  years=densYears(),common=F )$Latin_Name)
  SpecNames<-getPlantNames(object=NCRN[[input$densPark]], names=SpecTemp, in.style="Latin",
                           out.style=ifelse(input$densCommon,"common","Latin"))
  names(SpecTemp)<-SpecNames  
  SpecTemp<-SpecTemp[order(names(SpecTemp))]
})

output$densSpeciesControl<-renderUI({
  switch(input$densSpeciesType,
         Common= tags$div(title="# of species to display", 
            sliderInput(inputId="densTop",label="Number of species to display (in order of mean value):",
                min=1, max=10,value=5, format="##")
          ),
         Pick= if(is.null(input$densPark) || nchar(input$densPark)==0) {  return()  }
          else{
            tags$div(title="Click here to pick the species you want to graph",
              selectizeInput(inputId="densSpecies", label="Choose one or more species,
              backspace to remove", choices=densSpecList(),
                multiple=TRUE )
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
      Common=getPlantNames( object=NCRN[[input$densPark]], out.style="Latin", 
              in.style= ifelse(input$densCommon, "common", "Latin"),
              names= as.character(dens(object=NCRN[[input$densPark]], group=input$densGroup, years=densYears(),
                    values=input$densvalues, Total=F, common=input$densCommon)[order(-dens(object=NCRN[[input$densPark]],
                    group=input$densGroup, years=densYears(),values=input$densvalues, common=input$densCommon, 
                    Total=F)["Mean"]),][1:input$densTop,1] )),
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
         shseedlings="Shrub Seedling",
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
    col=if(input$CompareType=="None"){input$densBaseColor} else{c(input$densBaseColor,input$densCompareColor)}, 
    ylab=densYlabel(),
    main=DensTitle()
  )
})


############Density Plot Function

 
tempDensPlot<-reactive({
  if (is.null(input$densPark) || nchar(input$densPark)==0) {return()}
    else{
      validate(need(try(
        do.call(densplot,DensPlotArgs() )),
       "There is no data for this combination of choices. The type of plant you selected was not found in the park during those years."
       ))
      update(do.call(densplot, DensPlotArgs()), par.settings=list(fontsize=list(text=input$densFontSize,
                                                                                points=input$densPointSize )))
    }
})

output$DensPlot<-renderPlot(print(tempDensPlot()))

DensTableArgs<-reactive({
  list(
    object=DensPlotArgs()$object,
    group=DensPlotArgs()$densargs$group,
    years=DensPlotArgs()$densargs$years,
    values=DensPlotArgs()$densargs$values,
    common=DensPlotArgs()$densargs$common
  )
})

########### jpeg Plot download
output$densGraphDownload<-downloadHandler(
  filename=function(){paste(DensTitle(), ".jpeg", sep="")}, 
  content=function (file){
    jpeg(file,width=15,height=6,units="in",res=300, quality=100)
    print(tempDensPlot())
    dev.off()
  }
)

############# wmf plot download
output$densWmfDownload<-downloadHandler(
  filename=function(){paste(DensTitle(), ".wmf", sep="")}, 
  content=function (file){
    win.metafile(file,width=15,height=6)
    print(tempDensPlot())
    dev.off()
  }
)

################### Tables Tab

### Title for table
tempDensTableTitle<-reactive({
  validate(need(try(paste(getNames(NCRN[input$densPark],"long"),":",densTitleGroup(),densTitleValues(), 
                          paste0(as.character(input$densYear-3),"-",as.character(input$densYear),"(",densYlabel(),")") )), message=FALSE) )
  paste(getNames(NCRN[input$densPark],"long"),":",densTitleGroup(),densTitleValues(), 
        paste0(as.character(input$densYear-3),"-",as.character(input$densYear) ," (",densYlabel(),")") )
})
  
output$densTableTitle<-renderText({ tempDensTableTitle() })  

### Make Table

tempDensTable<-reactive({
  expr={
  validate(need(try(
    do.call(dens, DensTableArgs() ),
  ),
    "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years"
          ))
  TableOut<-do.call(dens,DensTableArgs())
  names(TableOut)<-c("Species",'Mean',"Lower 95% CI", "Upper 95% CI")
  return(TableOut)}
  
})

output$densTable<-renderDataTable(tempDensTable())

###Table Downlaod

output$densTableDownload<-downloadHandler(
  filename=function(){paste(tempDensTableTitle(), ".csv", sep="")}, 
  content=function (file){
    write.csv(tempDensTable(),file)
  }
)

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
    if(input$IVPart==FALSE){colors=input$IVBaseColor} else {colors=c(input$IVDensityColor,input$IVSizeColor,
                                                                     input$IVDistributionColor)},
    main=IVTitle(),
    par.settings=list(fontsize=list(text=input$IVFontSize))
  )
})


tempIVPlot<-reactive({ 
  if (is.null(input$IVPark) || nchar(input$IVPark)==0) {validate(need(input$IVPark, "Please select a park"))}
 else{ 
  validate(need(try(
      do.call(IVplot,IVPlotArgs() )),
      "There is no data for this combination of choices. The type of plant you selected was not found in the park during those years."
    ))
    update(do.call(IVplot, IVPlotArgs()), scales=list(cex=1.04))
  }
})

output$IVPlot<-renderPlot({tempIVPlot()})

### jpeg Plot Download
output$IVGraphDownload<-downloadHandler(
  filename=function(){paste(IVTitle(), ".jpeg", sep="")}, 
  content=function (file){
    jpeg(file,width=15,height=6,units="in",res=300, quality=100)
    print(tempIVPlot())
    dev.off()
  }
)

############# wmf plot download
output$IVWmfDownload<-downloadHandler(
  filename=function(){paste(IVTitle(), ".wmf", sep="")}, 
  content=function (file){
    win.metafile(file,width=15,height=6)
    print(tempIVPlot())
    dev.off()
  }
)


IVTableArgs<-reactive({
  list(
    object=IVPlotArgs()$object,
    group=IVPlotArgs()$IVargs$group, 
    years=IVPlotArgs()$IVargs$years, 
    common=IVPlotArgs()$IVargs$common
  )
})
##### IV Table 
## title

tempIVTableTitle<-reactive({ 
  validate(need(try(IVTitle()), message=FALSE) )
  IVTitle()
})

## Table
output$IVTableTitle<-renderText({tempIVTableTitle() })
  
tempIVTable<-reactive({
  validate(need(try(
    do.call(IV,IVTableArgs() )),
    "Please select a park"
  ))
  do.call(IV,IVTableArgs())
})
  
output$IVData<-renderDataTable({tempIVTable() })
###IV Table download

output$IVTableDownload<-downloadHandler(
  filename=function(){paste(tempIVTableTitle(), ".csv", sep="")}, 
  content=function (file){
    write.csv(tempIVTable(),file)
  }
)

####################################### Species list
## Species list park control
output$SpListParkControl<-renderUI({
  validate(
    need(ParkList, message=FALSE )
  )
  selectizeInput(inputId="SpListPark", choices=ParkList, label="Park:",
    options = list(placeholder='Choose a park', onInitialize = I('function() { this.setValue(""); }'))
  ) 
})


#### Species list plot control

output$SpListPlotControl <-renderUI({
  validate(
    need(input$SpListPark!="", message="Please select a Park")
  )
  selectizeInput(inputId="SpListPlot", choices=c("All Plots"="All", getPlotNames(NCRN[[input$SpListPark]],type="all")),
        label="Plots (optional)", multiple=TRUE, selected="All"
    )
})


SpListPlotUse<-reactive({
  if(length(input$SpListPlot)==0 || "All" %in%  input$SpListPlot ) return(NA) else return(input$SpListPlot)

})

LatinList<-reactive({
  validate(
  need(input$SpListPark, message=FALSE)  
  )
  unique(c(
    getPlants(object=NCRN[[input$SpListPark]], group="trees", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=NCRN[[input$SpListPark]], group="saplings",plots=SpListPlotUse())$Latin_Name,
    getPlants(object=NCRN[[input$SpListPark]], group="seedlings", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=NCRN[[input$SpListPark]], group="shrubs", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=NCRN[[input$SpListPark]], group="shseedlings", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=NCRN[[input$SpListPark]], group="vines", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=NCRN[[input$SpListPark]], group="herbs", plots=SpListPlotUse())$Latin_Name
    ))
})


decapitalize <- function(string) {     ########### used to hack around sorting/encoding issues
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}

CommonList<-reactive(decapitalize(getPlantNames(object=NCRN[[input$SpListPark]], names=LatinList(), out.style="common",in.style="Latin")))



###Make URL for and get data from NPSpecies
NPSpeciesURL<-reactive({paste0("http://irmaservices.nps.gov/v3/rest/npspecies/checklist/",input$SpListPark,"/Vascular%20Plant?format=Json")})

NPSpeciesList<-reactive({ fromJSON(NPSpeciesURL()) })


##Create Title for Table

output$SpeciesTableTitle<- renderText({
  switch(input$SpListType,
               Monitoring= "Species Found in the Monitoring Plots",
               NPSpecies="All Species Known from the Park")
}) 



##Create Table 
output$SpeciesTable<- renderDataTable({
 validate(
  need(input$SpListPark!="", message="Please choose a park")
  )

expr= switch(input$SpListType,
        Monitoring= data.frame(Latin=LatinList(),Common=CommonList())[order(LatinList()),],
        NPSpecies=data.frame("Latin" = NPSpeciesList()$ScientificNameFormatted, "Common"= NPSpeciesList()$CommonNames)
)}, 
options=list(order=c(0,"asc"))
)


})# end of shinyServer() function


