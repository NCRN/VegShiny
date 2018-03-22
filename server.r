library(shiny)
library(NPSForVeg)
library(leaflet)
library(lattice)
library(rgdal)
library(shinyjs)
library(jsonlite,pos=100)
library(httr)
library(dplyr)
library(DT)

#### Housekeeping prior to start of the server function ####
VegData<-switch(Network,
                ERMN=importERMN("./Data/ERMN"),
                MIDN=importMIDN("./Data/MIDN"),
                NCRN=importNCRN("./Data/NCRN"),
                NETN=importNETN("./Data/NETN"),
                SHEN=list(importSHEN("./Data/SHEN"))
)

names(VegData)<-getNames(VegData, name.class="code")
ParkList<-getNames(VegData,name.class="code")
names(ParkList)<-getNames(VegData)

ParkBounds<-read.csv("boundboxes.csv", as.is=TRUE)

##### Begin Server Function ####

shinyServer(function(input,output,session){
  
#### toggles ####
  observe ({
### Maps  
    onclick(id="AboutMapButton", expr= toggle(id="AboutMapPanel"))
    onclick(id="CloseAboutMap", expr= toggle(id="AboutMapPanel")) 
    onclick(id="CloseVideo", expr= toggle(id="VideoPanel")) 
### Graphs
    onclick(id="densGraphButton", expr=toggle(id="GraphOptionsPanel"))
    onclick(id="CloseDisplayOptions", expr= toggle(id="GraphOptionsPanel"))
    onclick(id="IVGraphButton", expr=toggle(id="IVOptionsPanel"))
    onclick(id="CloseIVDisplayOptions", expr= toggle(id="IVOptionsPanel"))
  })
  

####  Map Panel  ####

  
#### UI Controls ####

# Zoom control and zoom for map 
  
  output$ParkZoomControl<-renderUI({
    selectInput(inputId="ParkZoom",label=NULL,selectize=FALSE,
                choices=c("All Parks"=Network,ParkList) )
  })
  
  
#  Park Filter for species list control for map 
  
  output$MapParkControl<-renderUI({
    selectInput(inputId="MapPark", label="Filter species list by park",
                choices=c("All Parks"="All",ParkList) )
  })
  
# Data to display control for Map 
  ValuesUse<-reactive({
    switch(input$MapGroup,
           trees=,saplings=c(Abundance="count", "Basal Area"="size"),
           seedlings=,shseedlings=,shrubs=,vines=c(Abundance="count"),
           herbs=c("Percent Cover"="size")
           
    )
  })
  
  output$PlantValueControl<-renderUI({
    req(ValuesUse())
    selectInput(inputId="MapValues", label="Data to Map:", choices=ValuesUse())
    
  })

  
#### Calculations ####
# Load Layers
  withProgress(message="Loading...Please Wait", value=1,{
    Ecoregion<-readOGR(dsn="./Maps/Ecoregion.geojson")#,"OGRGeoJSON")
    Forested<-readOGR(dsn="./Maps/Forests.geojson")#,"OGRGeoJSON")
    Soil<-readOGR(dsn="./Maps/Soils.geojson")#,"OGRGeoJSON")
  })
  
# Map Years 
  MapYears<-reactive({ (input$MapYear-3) : input$MapYear  })   
  
  
# Map MetaData
  MapMetaData<-reactive({
    req(input$MapValues, input$MapGroup)
    MapLegend[[input$MapValues]][[input$MapGroup]] 
  })
  
# Data to plot on map - always for all parks 
  MapData<-reactive({
    req(input$MapSpecies=="All" | input$MapSpecies %in% getPlants(object=VegData, group=input$MapGroup, years=MapYears())$Latin_Name )
    req(input$MapGroup!="vines" | (input$MapGroup=="vines" & input$MapValues=="count"))
    
    P<-left_join(getPlots(VegData, years=MapYears(), output="dataframe", type="all") %>% 
        dplyr::select(Plot_Name,Unit_Code, Latitude, Longitude), getEvents(object=VegData, years=MapYears(), plot.type="all") %>% 
        dplyr::select(Plot_Name,Year=Event_Year), by="Plot_Name") %>% 
        mutate(Size=getArea(VegData[Unit_Code], group=input$MapGroup))
    
    if(input$MapGroup != "herbs"){
      return(P %>% 
               left_join(SiteXSpec(object=VegData, group=input$MapGroup, years=MapYears(), 
                       species= if(input$MapSpecies=="All") NA else input$MapSpecies, values=input$MapValues) %>% 
               dplyr::select(Plot_Name,Total), by="Plot_Name") %>% 
               mutate(Values=Total*10000/Size) %>% 
               {if(input$MapValues=="size") mutate(.,Values=Values/10000) else .} #Covert to m^2 from cm^2/ha for basal area
      )
    } 
    
    if(input$MapGroup == "herbs"){
      return(P %>% 
            mutate(Values=SiteXSpec(object=VegData,group=input$MapGroup, years=MapYears(),
                         species= if(input$MapSpecies=="All") NA else input$MapSpecies,
                         values=input$MapValues)$Total/getArea(VegData[Unit_Code], group=input$MapGroup, type="count"))
      )
    }
})
  
# Map Colors
  CircleColors<-reactive({
    req(MapMetaData()$Cuts)
    colorBin(palette=c("cyan","magenta4","orangered3"),domain=MapData()$Values, bins=c(MapMetaData()$Cuts+.001)) # colors for circles
  })  
  
  PolyColors<-colorRamp(c("aquamarine4","green","yellow","goldenrod4")) #colors for polygons
  
  
  
#### Render Map  ####
  output$VegMap<-renderLeaflet({
    leaflet() %>%
     setView(lng=mean(c(ParkBounds[ParkBounds$ParkCode==Network,]$LongE,ParkBounds[ParkBounds$ParkCode==Network,]$LongW)),
             lat=mean(c(ParkBounds[ParkBounds$ParkCode==Network,]$LatN,ParkBounds[ParkBounds$ParkCode==Network,]$LatS)),
                       zoom=8 ) %>%
    setMaxBounds(lng1=ParkBounds[ParkBounds$ParkCode==Network,]$LongE,lng2=ParkBounds[ParkBounds$ParkCode==Network,]$LongW,
                 lat1=ParkBounds[ParkBounds$ParkCode==Network,]$LatN, lat2=ParkBounds[ParkBounds$ParkCode==Network,]$LatS)
  })


# Make Attribution
  NPSAttrib<-HTML("<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> | 
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles' 
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>")


# add Monitoring plot data as circles - needs to be before layers or app hangs for some reason - new issue?
  observe({
    req(MapData()$Values)
    input$MapLayer #make sure Circles are always on top

    leafletProxy("VegMap") %>%
    clearGroup("Circles") %>%
    addCircles(data=MapData(), radius=15*as.numeric(input$PlotSize), group="Circles",
                 lng=MapData()$Longitude, lat=MapData()$Latitude,
                 layerId=MapData()$Plot_Name,  #This is the ID of the circle to match to other data
                 fillColor=CircleColors()(MapData()$Values),
                 color=CircleColors()(MapData()$Values),
                 fillOpacity=1
    )
  })

  
# #Add a tile layer
  observe({
    leafletProxy("VegMap") %>%
    clearTiles() %>%

    addTiles(group="Map", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.397cfb9a,nps.3cf3d4ab,nps.b0add3e6/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=8))%>%
    addTiles(group="Imagery", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2c589204,nps.25abf75b,nps.7531d30a/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=8)) %>%
    addTiles(group="Slate", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.9e521899,nps.17f575d9,nps.e091bdaf/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q", attribution=NPSAttrib, options=tileOptions(minZoom=8) ) %>%
     addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"), options=layersControlOptions(collapsed=T))
  })
  
# Add Circle legends 
  observe({
    leafletProxy("VegMap") %>%
      removeControl(layerId="CircleLegend") %>%
      addLegend(.,title=MapMetaData()$Title,
                colors=CircleColors()(MapMetaData()$Cuts[-1]-.001),
                labels=MapMetaData()$Labels,
                layerId="CircleLegend",
                opacity=1)
  })
  
  
# Species list control for map 
#List of names, elements are Latin names, names of elements are Latin or common
   
  MapSpecList<-reactive({
    req(input$MapPark, input$MapGroup)
    SpecTemp<-unique(getPlants(object=if(input$MapPark=="All") {VegData}  else {VegData[[input$MapPark]]} , group=input$MapGroup,
                               years=MapYears(),common=F )$Latin_Name)
    SpecNames<-getPlantNames(object=VegData[[1]], names=SpecTemp, in.style="Latin",out.style=ifelse(input$mapCommon,"common","Latin"))
    names(SpecTemp)<-SpecNames
    SpecTemp<-SpecTemp[order(tolower(names(SpecTemp)))]
    SpecTemp<-c("All Species"="All", SpecTemp)
  })

  output$MapSpeciesControl<-renderUI({
    req(input$MapPark, input$MapGroup)
    selectInput(inputId="MapSpecies", label="Choose a species", choices=c(MapSpecList() ))

  })
  

# Add GeoJSON polygon layer 
  
 observe({
   leafletProxy("VegMap") %>% {
     switch(input$MapLayer,
            None=clearGroup(.,group=c("Ecoregion","Forested","Soil")) %>% removeControl(.,"LayerLegend"),
            
             EcoReg=clearGroup(.,group=c("Forested","Soil") )%>% 
                addPolygons(., data=Ecoregion, group="Ecoregion", layerId=Ecoregion$MapClass, 
                            stroke=FALSE, 
                            fillOpacity=.65, color=colorFactor(palette=PolyColors, levels=Ecoregion$MapClass)(Ecoregion$MapClass)),
            
            ForArea=clearGroup(.,group=c("Ecoregion","Soil")) %>% 
              addPolygons(.,data=Forested, group="Forested", layerId=Forested$MapClass, stroke=FALSE, 
                          fillOpacity=.65, color=colorFactor("Greens",levels=Forested$MapClass)(Forested$MapClass)),
            
            Soil=clearGroup(.,group=c("Ecoregion","Forested")) %>% 
              addPolygons(.,data=Soil, group="Soil", layerId=Soil$MapClass, stroke=FALSE, 
                          fillOpacity=.65, color=colorFactor(PolyColors,levels=Soil$MapClass)(Soil$MapClass)) 
     )}
 })

  # Zoom the map
  
  observeEvent(input$MapZoom, {
    BoundsUse<-reactive({ as.numeric(ParkBounds[ParkBounds$ParkCode==input$ParkZoom,2:5]) })
    leafletProxy("VegMap") %>% fitBounds(lat1=BoundsUse()[1], lng1=BoundsUse()[2], lat2=BoundsUse()[3], lng2=BoundsUse()[4])
  })
  
  
# Add layer legends 
  
  observe({
    leafletProxy("VegMap") %>%  removeControl(layerId="LayerLegend") %>%
    { switch(input$MapLayer,
             None=NA,
             EcoReg= addLegend(.,title="Layer Legend",pal=colorFactor(PolyColors, levels=Ecoregion$MapClass),
                                  values=Ecoregion$MapClass, layerId="LayerLegend"),

             ForArea= addLegend(.,title="Layer Legend",pal=colorFactor("Greens",levels=Forested$MapClass),
                                values=Forested$MapClass,layerId="LayerLegend"),
             Soil= addLegend(.,title="Layer Legend",pal=colorFactor(PolyColors, levels=Soil$MapClass),
                               values=Soil$MapClass, layerId="LayerLegend")
      )}
  })
 
# Mouse Hover 

   observeEvent(input$VegMap_shape_mouseover, {

    ShapeOver<-input$VegMap_shape_mouseover
    selectedPlot <- MapData()[MapData()$Plot_Name == ShapeOver$id,]


    leafletProxy("VegMap") %>%
      clearPopups() %>% {
        switch(ShapeOver$group,
               Circles= addPopups(map=.,lat=ShapeOver$lat+.001, lng=ShapeOver$lng, layerId="MouseOverPopup",
                                  popup=paste0(
                                    h5(getNames(VegData[[selectedPlot$Unit_Code]], "long")),
                                    h6("Monitoring Plot:",selectedPlot$Plot_Name),
                                    h6("Year Monitored:", selectedPlot$Year),
                                    h6(names(MapSpecList()[MapSpecList()==input$MapSpecies]),":",format(signif(selectedPlot$Values,2),
                                                                                            big.mark=","), " ", MapMetaData()$Title),
                                    tags$h6("Click on plot to see full list")
                                  )
               )
        )}
  })

  observeEvent(input$VegMap_shape_mouseout,{    #clear popup when mouse leaves circle
               leafletProxy("VegMap") %>%
                 clearPopups()
  })

# Mouse Click 
 
   observeEvent(input$VegMap_shape_click, {          # user clicked on a shape
    ShapeClick<-input$VegMap_shape_click
    selectedPlot <- MapData()[MapData()$Plot_Name == ShapeClick$id,]

    if(
      class(try(SiteXSpec(object=VegData[[selectedPlot$Unit_Code]], group=input$MapGroup, years=selectedPlot$Year,
            plots=ShapeClick$id, common=input$mapCommon), silent=TRUE))=="try-error") {
      content<-as.character(tagList(tags$h6("None found on this plot")))
    } else {

      tempData<- if(input$MapGroup != "herbs" && input$MapValues != "size"){
        (10000/getArea(VegData[[selectedPlot$Unit_Code]],group=input$MapGroup,type="all")) *
        SiteXSpec(object=VegData[[selectedPlot$Unit_Code]],group=input$MapGroup, years=selectedPlot$Year,
        plots=ShapeClick$id, values=input$MapValues, common=input$mapCommon)[-1]

        } else {

        if(input$MapGroup != "herbs" && input$MapValues == "size"){
          (10000/getArea(VegData[[selectedPlot$Unit_Code]],group=input$MapGroup,type="all")) * (  #need to convert to m^2/ha from cm^2/ha
            SiteXSpec(object=VegData[[selectedPlot$Unit_Code]],group=input$MapGroup, years=selectedPlot$Year,
            plots=ShapeClick$id,values=input$MapValues,common=input$mapCommon)[-1])/10000
        } else {

          if(input$MapGroup == "herbs"){
            SiteXSpec(object=VegData[[selectedPlot$Unit_Code]], group=input$MapGroup, years=selectedPlot$Year,
            plots=ShapeClick$id,values=input$MapValues,common=input$mapCommon)[-1]/12
          }
        }
        }

      content<-paste0( h5(getNames(VegData[[selectedPlot$Unit_Code]],"long")),
                    h6("Monitoring Plot:",selectedPlot$Plot_Name),
                    h6("Year Monitored:",selectedPlot$Year),
                    h6("Species: ",MapMetaData()$Title),
                    tagList(tags$table(
                      mapply(FUN=function(Name,Value){
                        tags$tr(
                          tags$td(sprintf("%s:  ", Name)),
                          tags$td(align="right",sprintf("%s", format(signif(Value,2), big.mark=",")))
                        )
                      },
                      Name=names(tempData),
                      Value=unlist(tempData), SIMPLIFY=FALSE
                      )))

      )
    }

    leafletProxy("VegMap") %>%
      clearPopups() %>% {
      switch(ShapeClick$group,
             Circles= addPopups(map=.,lat=ShapeClick$lat+.001, lng=ShapeClick$lng, layerId="CircleClickPopup",popup=content),
             Ecoregion=, Forested=, Soil= addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$id)
      )}

  })

#### Plots Tab ####

#### Park Control for Density plot  ####
output$densParkControl<-renderUI({
  selectizeInput(inputId="densPark",choices=ParkList, label="Park:",
                 options = list(placeholder='Choose a park',
                                onInitialize = I('function() { this.setValue(""); }') )) 
})

#### Years to plot from control ####
densYears<-reactive({ (input$densYear-3):input$densYear })


##### Data to display control for density plot ####
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
#### Species Control (top species vs list) for density plots ####
densSpecList<-reactive({
  SpecTemp<-unique(getPlants(object=VegData[[input$densPark]], group=input$densGroup,  years=densYears(),common=F )$Latin_Name)
  SpecNames<-getPlantNames(object=VegData[[input$densPark]], names=SpecTemp, in.style="Latin",
                           out.style=ifelse(input$densCommon,"common","Latin"))
  names(SpecTemp)<-SpecNames  
  SpecTemp<-SpecTemp[order(names(SpecTemp))]
})

output$densSpeciesControl<-renderUI({
  switch(input$densSpeciesType,
         Common= tags$div(title="# of species to display", 
            sliderInput(inputId="densTop",label="Number of species to display (in order of mean value):",
                min=1, max=10,value=5, sep="", step=1, ticks=TRUE)
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


#### Control for comparison ####


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
                sliderInput(inputId="CompareYear", label="Display data from the 4 years ending:", min=Years$Start+Years$Range-1, 
                            max=Years$End, value=Years$End,  sep="", step=1,ticks=T))
  )
})
#### Need Compare species to keep the number of species to display to accepted number ####
CompareSpecies<-reactive({
  if(input$CompareType=="None") {NA} else {
    switch(input$densSpeciesType,
      Common=getPlantNames( object=VegData[[input$densPark]], out.style="Latin", 
              in.style= ifelse(input$densCommon, "common", "Latin"),
              names= as.character(dens(object=VegData[[input$densPark]], group=input$densGroup, years=densYears(),
                    values=input$densvalues, Total=F, common=input$densCommon)[order(-dens(object=VegData[[input$densPark]],
                    group=input$densGroup, years=densYears(),values=input$densvalues, common=input$densCommon, 
                    Total=F)["Mean"]),][1:input$densTop,1] )),
      Pick=input$densSpecies,
      All=NA
    )
  }
})


#### make compare and labels arguments for densplot() ####

DensCompare<-reactive({switch(input$CompareType,
    None=return(NA),
    Park=  if (is.null(input$ComparePark) || nchar(input$ComparePark)==0) {return(NA)}
      else{
        return(list(object=VegData[input$ComparePark], group=input$densGroup,  years=densYears(),
                    values=input$densvalues, species=CompareSpecies(), common=input$densCommon ))
      },
    "Growth Stage"=return(list(object=VegData[input$densPark], group=input$CompareGroup, years=densYears(),
                    values=input$densvalues, species=CompareSpecies(),common=input$densCommon )),
    Time=return(list(object=VegData[input$densPark], group=input$densGroup, years=c((input$CompareYear-3):input$CompareYear),
                     values=input$densvalues, species=CompareSpecies(),common=input$densCommon))
    )
})

DensLabelData<-data.frame(Name=c("trees","saplings","seedlings","shrubs","shseedlings","herbs","vines"), Label=c("Trees","Saplings","Tree Seedlings", "Shrubs","Shrub Seedlings","Understory Plants","Vines in Trees"), stringsAsFactors=FALSE)

DensLabels<-reactive({switch(input$CompareType,
    None=return(NA),
    Park=  if (is.null(input$ComparePark) || nchar(input$ComparePark)==0) {return(NA)}
            else{return(c(getNames(object=VegData[input$densPark],"short"), getNames(object=VegData[input$ComparePark], "short") ) )},
    "Growth Stage"=if (is.null(input$CompareGroup) || nchar(input$CompareGroup)==0) {return(NA)}
            else{ return(c(DensLabelData[DensLabelData$Name==input$densGroup,]$Label,
                           DensLabelData[DensLabelData$Name==input$CompareGroup,]$Label))},
    Time=if (is.null(input$CompareYear) || nchar(input$CompareYear)==0) {return(NA)}
              else{return(c( paste0(as.character(input$densYear-3),"-",as.character(input$densYear)),
                  paste0(as.character(input$CompareYear-3),"-",as.character(input$CompareYear)) ))}
  ) 
})

#### Y axis labels for density plot ####
densYlabel<-reactive({
  switch(input$densvalues,
    count=switch(input$densGroup,
      trees="Trees / ha",
      saplings="Saplings / ha",
      seedlings="Tree seedlings / ha",
      shrubs="Shrubs / ha",
      shseedlings="Shrub seedlings / ha",
      vines="Vines on Trees / ha"
    ),
    size=switch(input$densGroup,
      trees=,saplings="Basal area cm2/ ha",
      herbs="Percent Cover"
    ),
    presab="Proportion of Plots Occupied"
  )
})

#### Title for density plot ####
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
         None=  return(paste(getNames(VegData[input$densPark],"long"),":",densTitleGroup(),densTitleValues(), 
               paste0(as.character(input$densYear-3),"-",as.character(input$densYear)) )),
         Park=return(paste(getNames(VegData[input$densPark],"long"),"vs.",getNames(VegData[input$ComparePark],"long"),":",
                           densTitleGroup(),densTitleValues(), 
                           paste0(as.character(input$densYear-3),"-",as.character(input$densYear)) )),
         "Growth Stage"= return(paste(getNames(VegData[input$densPark],"long"),":",densTitleGroup(),"vs.",
                                      compareTitleGroup(), densTitleValues(), 
                                      paste0(as.character(input$densYear-3),"-",as.character(input$densYear)) )),
         Time=return(paste(getNames(VegData[input$densPark],"long"),":",densTitleGroup(),densTitleValues(), 
                           paste0(as.character(input$densYear-3),"-",as.character(input$densYear)),"vs.",
                           paste0(as.character(input$CompareYear-3),"-",as.character(input$CompareYear)) ))
  )
})


#### All arguments for densityPlot ####
DensPlotArgs<-reactive({
  list(
    object=VegData[input$densPark],
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


#### Density Plot Function ####

 
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

##### jpeg Plot download ####
output$densGraphDownload<-downloadHandler(
  filename=function(){paste(DensTitle(), ".jpeg", sep="")}, 
  content=function (file){
    jpeg(file,width=15,height=6,units="in",res=300, quality=100)
    print(tempDensPlot())
    dev.off()
  }
)

##### wmf plot download ####
output$densWmfDownload<-downloadHandler(
  filename=function(){paste(DensTitle(), ".png", sep="")}, 
  content=function (file){
    png(file,width=15,height=6,res=300, units="in")
    print(tempDensPlot())
    dev.off()
  }
)

#### Tables Tab ####
#### Title for table ####
tempDensTableTitle<-reactive({
  validate(need(try(paste(getNames(VegData[input$densPark],"long"),":",densTitleGroup(),densTitleValues(), 
                          paste0(as.character(input$densYear-3),"-",as.character(input$densYear),"(",densYlabel(),")") )), message=FALSE) )
  paste(getNames(VegData[input$densPark],"long"),":",densTitleGroup(),densTitleValues(), 
        paste0(as.character(input$densYear-3),"-",as.character(input$densYear) ," (",densYlabel(),")") )
})
  
output$densTableTitle<-renderText({ tempDensTableTitle() })  

#### Make Table ####

tempDensTable<-reactive({
  expr={
  validate(need(try(
    do.call(dens, DensTableArgs() )
  ),
    "There is no data for this combination of choices. Either you need to select a park, or the type of plant you selected was not found in the park during those years"
          ))
  TableOut<-do.call(dens,DensTableArgs())
  names(TableOut)<-c("Species",'Mean',"Lower 95% CI", "Upper 95% CI")
  return(TableOut)}
  
})

output$densTable<-renderDataTable(tempDensTable())

#### Table Download ####

output$densTableDownload<-downloadHandler(
  filename=function(){paste(tempDensTableTitle(), ".csv", sep="")}, 
  content=function (file){
    write.csv(tempDensTable(),file)
  }
)

#### IV Plots ####
#### Park Control for IVPlot  ####
output$IVParkControl<-renderUI({
  selectizeInput(inputId="IVPark",choices=ParkList, label="Park:",
                 options = list(placeholder='Choose a park',
                                onInitialize = I('function() { this.setValue(""); }') )) 
})

#### All arguments for IVPlot ####
IVYears<-reactive({ (input$IVYear-3):input$IVYear})

#### Title for IVPlot ####
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
  return(paste(getNames(VegData[input$IVPark],"long"),":","\n", IVTitleGroup(),"Importance Values", 
                             paste0(as.character(input$IVYear-3),"-",as.character(input$IVYear)) ))
         
})

IVPlotArgs<-reactive({
  list(
    object=VegData[input$IVPark],
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

#### jpeg Plot Download ####
output$IVGraphDownload<-downloadHandler(
  filename=function(){paste(IVTitle(), ".jpeg", sep="")}, 
  content=function (file){
    jpeg(file,width=15,height=6,units="in",res=300, quality=100)
    print(tempIVPlot())
    dev.off()
  }
)

#### wmf plot download ####
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
#### IV Table ####
#### title ####

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
#### IV Table download ####

output$IVTableDownload<-downloadHandler(
  filename=function(){paste(tempIVTableTitle(), ".csv", sep="")}, 
  content=function (file){
    write.csv(tempIVTable(),file)
  }
)

#### Species list ####
#### Species list park control ####
output$SpListParkControl<-renderUI({
  validate(
    need(ParkList, message=FALSE )
  )
  selectizeInput(inputId="SpListPark", choices=ParkList, label="Park:",
    options = list(placeholder='Choose a park', onInitialize = I('function() { this.setValue(""); }'))
  ) 
})


#### Species list plot control ####

output$SpListPlotControl <-renderUI({
  validate(
    need(input$SpListPark!="", message="Please select a Park")
  )
  selectizeInput(inputId="SpListPlot", choices=c("All Plots"="All", getPlotNames(VegData[[input$SpListPark]],type="all")),
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
    getPlants(object=VegData[[input$SpListPark]], group="trees", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=VegData[[input$SpListPark]], group="saplings",plots=SpListPlotUse())$Latin_Name,
    getPlants(object=VegData[[input$SpListPark]], group="seedlings", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=VegData[[input$SpListPark]], group="shrubs", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=VegData[[input$SpListPark]], group="shseedlings", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=VegData[[input$SpListPark]], group="vines", plots=SpListPlotUse())$Latin_Name,
    getPlants(object=VegData[[input$SpListPark]], group="herbs", plots=SpListPlotUse())$Latin_Name
    ))
})



decapitalize <- function(string) {     ########### used to hack around sorting/encoding issues
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}

CommonList<-reactive(decapitalize(getPlantNames(object=VegData[[input$SpListPark]], names=LatinList(), out.style="common",in.style="Latin")))


MonitoringList<-reactive({ 
  tbl_df( data.frame('Latin.Name'=LatinList(),'Common.Name'=CommonList())) %>% 
  arrange (Common.Name) %>% 
  rename('Latin Name'=Latin.Name, 'Common Name'=Common.Name) %>% 
  .[,c(2,1)]
})

###Make URL for and get data from NPSpecies
NPSpeciesURL<-reactive({paste0("https://irmaservices.nps.gov/v3/rest/npspecies/checklist/",input$SpListPark,"/Vascular%20Plant?format=Json")})

NPSpeciesList<-reactive({
  fromJSON(NPSpeciesURL()) %>% 
  dplyr::select(CommonNames,ScientificName,Occurrence) %>% 
  arrange(CommonNames) %>% 
  rename("Latin Name"=ScientificName, "Common Name"=CommonNames)
})  
  


##Create Title for Table

output$SpeciesTableTitle<- renderText({
  switch(input$SpListType,
               Monitoring= "Species Found in the Monitoring Plots",
               NPSpecies="All Species Known from the Park")
}) 



##Create Table 
output$SpeciesTable<- DT::renderDataTable({
 validate(
  need(input$SpListPark!="", message="Please choose a park")
  )
  
  datatable(rownames=F, caption="Species List", class="display compact", selection="single",
            data=switch(input$SpListType,
                        Monitoring= MonitoringList(),
                        NPSpecies=NPSpeciesList()
            )
  ) %>% 
  formatStyle('Latin Name', fontStyle='italic' )
})


})# end of shinyServer() function


