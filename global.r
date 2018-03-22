#### Network specific settings ####
Network<-"NCRN"
NetworkURL<-switch(Network,
                   ERMN=, MIDN=, NCRN=, NETN = paste0('https://science.nature.nps.gov/im/units/',tolower(Network),'/index.cfm'),
                   SHEN='https://www.nps.gov/shen/index.htm'
)

PlantTypes<-switch(Network,
    ERMN=list(Trees='trees',Saplings="saplings","Tree seedlings"="seedlings", "Understory plants"="herbs"),
    MIDN=list(Trees='trees',Saplings="saplings","Tree seedlings"="seedlings", "Understory plants"="herbs","Vines on Trees"="vines"),
    NCRN=list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings", Shrubs="shrubs", "Shrub seedlings"="shseedlings",
                "Understory plants"="herbs","Vines on Trees"="vines"),
    NETN=list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings", "Understory plants"="herbs","Vines on Trees"="vines"),
    SHEN=list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings", Shrubs="shrubs", "Shrub seedlings"="shseedlings",
               "Understory plants"="herbs")
)

IVPlantTypes<-switch(Network,  #needed as not all plants have an IV
    ERMN=list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings"),
    MIDN=list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings"),
    NCRN=list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings","Shrub seedlings"="shseedlings"),
    NETN=list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings"),
    SHEN=list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings","Shrub seedlings"="shseedlings")
  )

Years<-switch(Network,
             ERMN=list(Start=2007, End=2017, Range=4),
             MIDN=list(Start=2007, End=2017, Range=4),
             NCRN=list(Start=2006, End=2017, Range=4),
             NETN=list(Start=2006, End=2017, Range=4),
             SHEN=list(Start=2003, End=2017, Range=4)
  )

ExtraLayers<-switch(Network,
                    ERMN=c(None="None"),
                    MIDN=c(None="None"),
                    NCRN=c(None="None", "EcoRegions"="EcoReg","Forested Areas"="ForArea","Soil Map "="Soil"),
                    NETN=c(None="None"),
                    SHEN=c(None="None")
  )

ProjectInfo<-switch(Network,
                    ERMN=includeHTML("./www/InformationERMN.html"),
                    MIDN=includeHTML("./www/InformationMIDN.html"),
                    NCRN=includeHTML("./www/Information.html"),
                    NETN=includeHTML("./www/InformationNETN.html"),
                    SHEN=HTML("<h1>Add Me!</h1>")
)

Citations<-switch(Network,
                    ERMN=includeHTML("./www/CitationsERMN.html"),
                    MIDN=includeHTML("./www/CitationsMIDN.html"),
                    NCRN=includeHTML("./www/Citations.html"),
                    NETN=includeHTML("./www/CitationsNETN.html"),
                    SHEN=HTML("<h1>Add Me!</h1>")
)


#### Colors ####
GraphColors<-read.csv("./Data/colors.csv", header=T, as.is=T)
ColorNames<-GraphColors$Rcolor
names(ColorNames)<-GraphColors$DisplayColor
BlueOr<-colorRampPalette(c("cyan","magenta4","orangered3")) # colors for circles
AquaYel<-colorRampPalette(c("aquamarine4","green","yellow","goldenrod4")) #colors for polygons

#### Legend for Map ####
MapLegend<-list(
  count=list(
    trees=list(
      Title="Trees / hectare",
      Cuts=c(-1,0,25,50,100,200,400,600,100000),
      Labels=c("0","0 - 25","25 - 50","50 - 100","100 - 200","200 - 400","400 - 600","600+")
    ),
    saplings=list(
      Title="Saplings / hectare",
      Cuts=c(-1,0,150,300,600,1200,1800,2400,100000),
      Labels=c("0","0 - 150", "150 - 300", "300 - 600","600 - 1200","1200 - 1800","1800 - 2400","2400+")
    ),
    seedlings=list(
      Title="Tree seedlings / hectare",
      Cuts=c(-1,0,2500,5000,7500,10000,20000,40000,100000000),
      Labels=c("0","0 - 2500","2500 - 5000","5000 - 7500","7500 - 10,000","10,000 - 20,000","20,000 - 40,000","40,000+")
    ),
    shrubs=list(
      Title="Shrubs / hectare",
      Cuts=c(-1,0,250,500,1000,1500,3000,6000,100000),
      Labels=c("0","0 - 250","250 - 500","500 - 1000","1000 - 1500", "1500 - 3000", "3000 - 6000", "6000+")
    ),
    shseedlings=list(
      Title="Shrub seedlings / hectare",
      Cuts=c(-1,0,2500,5000,7500,10000,20000,40000,100000000),
      Labels=c("0","0 - 2500","2500 - 5000","5000 - 7500","7500 - 10,000","10,000 - 20,000","20,000 - 40,000","40,000+")
    ),
    vines=list(
      Title="Vines on trees / hectare",
      Cuts=c(-1,0,25,50,100,250,500,1000,100000),
      Labels=c("0","0 - 25","25 - 50","50 - 100","100 - 250", "250 - 500", " 500 - 1000", "1000+")
    )
  ),
  size=list(   
    trees=list(
      Title="Basal area m2 / hectare",
      Cuts=c(-1,0,0.5, 1.5, 7.5,15,30,45,10000000),
      Labels=c("0","0 - 0.5","0.5 - 1.5","1.5 - 7.5","7.5 - 15","15 - 30","30 - 45","45+")
    ),
    saplings=list(
      Title="Basal Area m2 / hectare",
      Cuts=c(-1,0,0.25,0.5,1.0,2.0,3.0, 4.0, 10000000),
      Labels=c("0","0 - 0.25","0.25 - 0.5","0.5 - 1.0","1.0 - 2.0", "2.0 -3.0", "3.0 - 4.0","4.0+")
    ),
    herbs=list(
      Title="Percent Cover",
      Cuts=c(-1,0,1,5,10,25,50,75,1000),
      Labels=c("0%","0 - 1%","1 - 5%","5 - 10%","10 - 25%","25 - 50%","50 - 75%","75%+")
    )
  )
)

