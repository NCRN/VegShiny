library(rgdal)

AquaYel<-colorRampPalette(c("aquamarine4","green","yellow","goldenrod4")) #colors for polygons

PolyOpts<-function(Map)  sapply(X=as.numeric(factor(Map@data$MapClass)), Y=length(unique(Map@data$MapClass)), FUN=function(X,Y) {
    list(color=AquaYel(Y)[X])
  })
#MapClass<-c(MapClass,if(!is.na(MapIn@data[i,"MapClass"])){as.character(MapIn@data[i,"MapClass"])} else("Not classified")  )




EcoRegTemp=readOGR(dsn="./geojson",layer="EcoRegionFinal")

EcoRegTemp@data$CentLng<-coordinates(EcoRegTemp)[,1]
EcoRegTemp@data$CentLat<-coordinates(EcoRegTemp)[,2]


writeOGR(obj=EcoRegTemp, dsn="./geojson/EcoRegTemp", layer="OGRGeoJSON",driver="GeoJSON" )
dput(EcoRegTemp@data, file="./geojson/EcoRegData.txt")

EcoReg<-readChar("./geojson/EcoRegTemp", file.info("./geojson/EcoRegTemp")$size)
EcoRegColors<-PolyOpts(EcoRegTemp)
EcoStyle<-paste0('"style": {"weight": 0, "fillOpacity": 0.40, "fillColor": "',EcoRegColors, '"}, ')

regmatches(EcoReg,gregexpr(pattern="[0123456789], \"properties\": \\{ \\K", text=EcoReg,perl=T)) <- list(EcoStyle)
writeChar(object=EcoReg,con="./geojson/EcoReg")





ForestTemp=readOGR(dsn="./geojson",layer="ForestFinal")

writeOGR(obj=ForestTemp, dsn="./geojson/ForestTemp", layer="OGRGeoJSON",driver="GeoJSON" )
dput(ForestTemp@data, file="./geojson/ForestData.txt")

Forest<-readChar("./geojson/ForestTemp", file.info("./geojson/ForestTemp")$size)
ForestColors<-PolyOpts(ForestTemp)
ForestStyle<-paste0('"style": {"weight": 0, "fillOpacity": 0.60, "fillColor": "',ForestColors, '"}, ')

regmatches(Forest,gregexpr(pattern="[0123456789], \"properties\": \\{ \\K", text=Forest,perl=T)) <- list(ForestStyle)
writeChar(object=Forest,con="./geojson/Forest")



SoilTemp=readOGR(dsn="./geojson",layer="SoilFinal")
SoilTemp@data$MapClass<-as.character(SoilTemp@data$MapClass)
SoilTemp@data$MapClass[is.na(SoilTemp@data$MapClass)]<-c("Not Classified")
writeOGR(obj=SoilTemp, dsn="./geojson/SoilTemp", layer="OGRGeoJSON",driver="GeoJSON" )
dput(SoilTemp@data, file="./geojson/SoilData.txt")

Soil<-readChar("./geojson/SoilTemp", file.info("./geojson/SoilTemp")$size)
SoilColors<-PolyOpts(SoilTemp)
SoilStyle<-paste0('"style": {"weight": 0, "fillOpacity": 0.60, "fillColor": "',SoilColors, '"}, ')

regmatches(Soil,gregexpr(pattern="[0123456789], \"properties\": \\{ \\K", text=Soil, perl=T)) <- list(SoilStyle)
writeChar(object=Soil,con="./geojson/Soil")


