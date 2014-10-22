library(rgdal)

AquaYel<-colorRampPalette(c("aquamarine4","green","yellow","goldenrod4")) #colors for polygons

PolyOpts<-function(Map)  sapply(X=as.numeric(factor(Map@data$MapClass)), Y=length(unique(Map@data$MapClass)), FUN=function(X,Y) {
    list(color=AquaYel(Y)[X])
  })
#MapClass<-c(MapClass,if(!is.na(MapIn@data[i,"MapClass"])){as.character(MapIn@data[i,"MapClass"])} else("Not classified")  )

setwd("T:/I&M/MONITORING/Forest_Vegetation/Visualizer/VegShiny/geojson")


EcoRegTemp=readOGR(dsn=getwd(),layer="Ecoregions_Omernick_Level3_WGS84")

EcoRegTemp@data$CentLng<-coordinates(EcoRegTemp)[,1]
EcoRegTemp@data$CentLat<-coordinates(EcoRegTemp)[,2]
EcoRegTemp@data$AREA<-EcoRegTemp@data$PERIMETER<-EcoRegTemp@data$OLDECOCODE<-EcoRegTemp@data$ECO_NAME<-EcoRegTemp@data$State<-EcoRegTemp@data$NAME<-
  EcoRegTemp@data$SHAPE_Leng<-EcoRegTemp@data$SHAPE_Area<-NULL


writeOGR(obj=EcoRegTemp, dsn="L:/geojson/EcoRegTemp", layer="OGRGeoJSON",driver="GeoJSON" )
dput(EcoRegTemp@data, file="EcoRegData.txt")

EcoReg<-readChar("EcoRegTemp", file.info("EcoRegTemp")$size)
EcoRegColors<-PolyOpts(EcoRegTemp)
EcoStyle<-paste0('"style": {"weight": 0, "fillOpacity": 0.40, "fillColor": "',EcoRegColors, '"}, ')

regmatches(EcoReg,gregexpr(pattern="[0123456789], \"properties\": \\{ \\K", text=EcoReg,perl=T)) <- list(EcoStyle)
writeChar(object=EcoReg,con="EcoReg")





ForestTemp=readOGR(dsn=getwd(),layer="Forest_NLCD_2011_Clip_WGS84_Simplified")

ForestTemp@data$CentLng<-coordinates(ForestTemp)[,1]
ForestTemp@data$CentLat<-coordinates(ForestTemp)[,2]
ForestTemp@data$gridcode<-ForestTemp@data$Shape_Leng<-ForestTemp@data$Shape_Area<-ForestTemp@data$ORIG_FID<-
  ForestTemp@data$Id<-ForestTemp@data$OBJECTID<-NULL

writeOGR(obj=ForestTemp, dsn="L:/geojson/ForestTemp", layer="OGRGeoJSON",driver="GeoJSON" )
dput(ForestTemp@data, file="ForestData.txt")

Forest<-readChar("ForestTemp", file.info("ForestTemp")$size)
ForestColors<-PolyOpts(ForestTemp)
ForestStyle<-paste0('"style": {"weight": 0, "fillOpacity": 0.60, "fillColor": "',ForestColors, '"}, ')

regmatches(Forest,gregexpr(pattern="[0123456789], \"properties\": \\{ \\K", text=Forest,perl=T)) <- list(ForestStyle)
writeChar(object=Forest,con="Forest")



SoilTemp=readOGR(dsn=getwd(),layer="SOIL_TaxonomySSURGO_NCRN_py_WGS84_Dissolved_SinglePart")
SoilTemp@data$CentLng<-coordinates(SoilTemp)[,1]
SoilTemp@data$CentLat<-coordinates(SoilTemp)[,2]
SoilTemp@data$MapClass<-as.character(SoilTemp@data$MapClass)
SoilTemp@data$MapClass[is.na(SoilTemp@data$MapClass)]<-c("Not Classified")
SoilTemp@data$taxorder<-as.character(SoilTemp@data$taxorder)
SoilTemp@data$taxorder[is.na(SoilTemp@data$taxorder)]<-c("Not Classified")



writeOGR(obj=SoilTemp, dsn="L:/geojson/SoilTemp", layer="OGRGeoJSON",driver="GeoJSON" )
dput(SoilTemp@data, file="SoilData.txt")

Soil<-readChar("SoilTemp", file.info("SoilTemp")$size)
SoilColors<-PolyOpts(SoilTemp)
SoilStyle<-paste0('"style": {"weight": 0, "fillOpacity": 0.60, "fillColor": "',SoilColors, '"}, ')

regmatches(Soil,gregexpr(pattern="[0123456789], \"properties\": \\{ \\K", text=Soil, perl=T)) <- list(SoilStyle)
writeChar(object=Soil,con="Soil")


