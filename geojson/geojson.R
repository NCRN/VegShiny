library(rgdal)

AQUAYEL<-grDevices::colorRampPalette(base::c("aquamarine4","green","yellow","goldenrod4")) #colors for polygons

PolyOpts<-function(Map)  base::sapply(X=base::as.numeric(base::factor(Map@data$MapClass)), Y=base::length(base::unique(Map@data$MapClass)), FUN=function(X,Y) {
    base::list(color=AQUAYEL(Y)[X])
  })
#MapClass<-base::c(MapClass,if(!base::is.na(MapIn@data[i,"MapClass"])){base::as.character(MapIn@data[i,"MapClass"])} else("Not classified")  )




EcoRegTemp=rgdal::readOGR(dsn="./geojson",layer="EcoRegionFinal")

EcoRegTemp@data$CentLng<-sp::coordinates(EcoRegTemp)[,1]
EcoRegTemp@data$CentLat<-sp::coordinates(EcoRegTemp)[,2]


rgdal::writeOGR(obj=EcoRegTemp, dsn="./geojson/EcoRegTemp", layer="OGRGeoJSON",driver="GeoJSON" )
base::dput(EcoRegTemp@data, file="./geojson/EcoRegData.txt")

EcoReg<-base::readChar("./geojson/EcoRegTemp", base::file.info("./geojson/EcoRegTemp")$size)
EcoRegColors<-PolyOpts(EcoRegTemp)
EcoStyle<-base::paste0('"style": {"weight": 0, "fillOpacity": 0.40, "fillColor": "',EcoRegColors, '"}, ')

base::regmatches(EcoReg,base::gregexpr(pattern="[0123456789], \"properties\": \\{ \\K", text=EcoReg,perl=T)) <- base::list(EcoStyle)
base::writeChar(object=EcoReg,con="./geojson/EcoReg")





ForestTemp=rgdal::readOGR(dsn="./geojson",layer="ForestFinal")

rgdal::writeOGR(obj=ForestTemp, dsn="./geojson/ForestTemp", layer="OGRGeoJSON",driver="GeoJSON" )
base::dput(ForestTemp@data, file="./geojson/ForestData.txt")

Forest<-base::readChar("./geojson/ForestTemp", base::file.info("./geojson/ForestTemp")$size)
ForestColors<-PolyOpts(ForestTemp)
ForestStyle<-base::paste0('"style": {"weight": 0, "fillOpacity": 0.60, "fillColor": "',ForestColors, '"}, ')

base::regmatches(Forest,base::gregexpr(pattern="[0123456789], \"properties\": \\{ \\K", text=Forest,perl=T)) <- base::list(ForestStyle)
base::writeChar(object=Forest,con="./geojson/Forest")



SoilTemp=rgdal::readOGR(dsn="./geojson",layer="SoilFinal")
SoilTemp@data$MapClass<-base::as.character(SoilTemp@data$MapClass)
SoilTemp@data$MapClass[base::is.na(SoilTemp@data$MapClass)]<-base::c("Not Classified")
rgdal::writeOGR(obj=SoilTemp, dsn="./geojson/SoilTemp", layer="OGRGeoJSON",driver="GeoJSON" )
base::dput(SoilTemp@data, file="./geojson/SoilData.txt")

Soil<-base::readChar("./geojson/SoilTemp", base::file.info("./geojson/SoilTemp")$size)
SoilColors<-PolyOpts(SoilTemp)
SoilStyle<-base::paste0('"style": {"weight": 0, "fillOpacity": 0.60, "fillColor": "',SoilColors, '"}, ')

base::regmatches(Soil,base::gregexpr(pattern="[0123456789], \"properties\": \\{ \\K", text=Soil, perl=T)) <- base::list(SoilStyle)
base::writeChar(object=Soil,con="./geojson/Soil")


