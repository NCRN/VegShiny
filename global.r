#### Network specific settings ####
NETWORK<-"NCRN"
NETWORKURL<-base::switch(NETWORK,
                   ERMN=, MIDN=, NCRN=, NETN = base::paste0('https://www.nps.gov/im/',base::tolower(NETWORK)),
                   SHEN='https://www.nps.gov/shen/index.htm'
)

PLANTTYPES<-base::switch(NETWORK,
    ERMN=base::list(Trees='trees',Saplings="saplings","Tree seedlings"="seedlings", "Understory plants"="herbs","Coarse Woody Debris"='cwd'),
    MIDN=base::list(Trees='trees',Saplings="saplings","Tree seedlings"="seedlings", "Understory plants"="herbs","Vines on Trees"="vines",
              "Coarse Woody Debris"='cwd'),
    NCRN=base::list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings", Shrubs="shrubs", "Shrub seedlings"="shseedlings",
                "Understory plants"="herbs","Vines on Trees"="vines"),
    NETN=base::list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings", "Understory plants"="herbs","Vines on Trees"="vines", 
              "Coarse Woody Debris"='cwd'),
    SHEN=base::list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings", Shrubs="shrubs", "Shrub seedlings"="shseedlings",
               "Understory plants"="herbs")
)

PLANTSLOTLOOKUP<-base::switch(NETWORK,
    NCRN=base::c(
      trees = "Trees",
      saplings = "Saplings",
      seedlings = "Tree seedlings",
      shrubs = "Shrubs",
      shseedlings = "Shrub seedlings",
      herbs = "Understory plants",
      vines = "Vines on Trees",
      cwd = "Coarse Woody Debris"
    )
)

IVPLANTTYPES<-base::switch(NETWORK,  #needed as not all plants have an IV
    ERMN=base::list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings"),
    MIDN=base::list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings"),
    NCRN=base::list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings","Shrub seedlings"="shseedlings"),
    NETN=base::list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings"),
    SHEN=base::list(Trees="trees",Saplings="saplings","Tree seedlings"="seedlings","Shrub seedlings"="shseedlings")
  )

YEARS<-base::switch(NETWORK,
             ERMN=base::list(Start=2007, End=2017, Range=4),
             MIDN=base::list(Start=2007, End=2017, Range=4),
             NCRN=base::list(Start=2006, End=2017, Range=4),
             NETN=base::list(Start=2006, End=2017, Range=4),
             SHEN=base::list(Start=2003, End=2017, Range=4)
  )

EXTRALAYERS<-base::switch(NETWORK,
                    ERMN=base::c(None="None"),
                    MIDN=base::c(None="None"),
                    NCRN=base::c(None="None", "EcoRegions"="EcoReg","Forested Areas"="ForArea","Soil Map "="Soil"),
                    NETN=base::c(None="None"),
                    SHEN=base::c(None="None")
  )

PROJECTINFO<-base::switch(NETWORK,
                    ERMN=htmltools::includeHTML("www/InformationERMN.html"),
                    MIDN=htmltools::includeHTML("www/InformationMIDN.html"),
                    NCRN=htmltools::includeHTML("www/Information.html"),
                    NETN=htmltools::includeHTML("www/InformationNETN.html"),
                    SHEN=htmltools::HTML("./www/InformationSHEN.html")
)

CITATIONS<-base::switch(NETWORK,
                    ERMN=htmltools::includeHTML("www/CitationsERMN.html"),
                    MIDN=htmltools::includeHTML("www/CitationsMIDN.html"),
                    NCRN=htmltools::includeHTML("www/Citations.html"),
                    NETN=htmltools::includeHTML("www/CitationsNETN.html"),
                    SHEN=htmltools::HTML("./www/CitationsSHEN.html")
)


#### Colors ####
GRAPHCOLORS<-utils::read.csv("./Data/colors.csv", header=T, as.is=T)
COLORNAMES<-GRAPHCOLORS$Rcolor
base::names(COLORNAMES)<-GRAPHCOLORS$DisplayColor
BLUEOR<-grDevices::colorRampPalette(base::c("cyan","magenta4","orangered3")) # colors for circles
AQUAYEL<-grDevices::colorRampPalette(base::c("aquamarine4","green","yellow","goldenrod4")) #colors for polygons

#### Legend for Map ####
MAPLEGEND <- list(
  count = base::list(
    trees = base::list(
      Title  = "Trees / hectare",
      Cuts   = base::c(0, 25, 50, 100, 200, 400, 600, 100000),
      Labels = base::c("1–25", "26–50", "51–100", "101–200", "201–400", "401–600", "600+")),
    saplings = base::list(
      Title  = "Saplings / hectare",
      Cuts   = base::c(0, 150, 300, 600, 1200, 1800, 2400, 100000),
      Labels = base::c("1–150", "151–300", "301–600", "601–1,200", "1,201–1,800", "1,801–2,400", "2,400+")),
    seedlings = base::list(
      Title  = "Tree seedlings / hectare",
      Cuts   = base::c(-1, 0, 2500, 5000, 7500, 10000, 20000, 40000, 100000000),
      Labels = base::c("0", "1–2,500", "2,501–5,000", "5,001–7,500", "7,501–10,000", "10,001–20,000", "20,001–40,000", "40,000+")),
    shrubs = base::list(
      Title  = "Shrubs / hectare",
      Cuts   = base::c(-1, 0, 250, 500, 1000, 1500, 3000, 6000, 100000),
      Labels = base::c("0", "1–250", "251–500", "501–1,000", "1,001–1,500", "1,501–3,000", "3,001–6,000", "6,000+")),
    shseedlings = base::list(
      Title  = "Shrub seedlings / hectare",
      Cuts   = base::c(-1, 0, 2500, 5000, 7500, 10000, 20000, 40000, 100000000),
      Labels = base::c("0", "1–2,500", "2,501–5,000", "5,001–7,500", "7,501–10,000", "10,001–20,000", "20,001–40,000", "40,000+")),
    vines = base::list(
      Title  = "Vines on trees / hectare",
      Cuts   = base::c(-1, 0, 25, 50, 100, 250, 500, 1000, 100000),
      Labels = base::c("0", "1–25", "26–50", "51–100", "101–250", "251–500", "501–1,000", "1,000+"))),
  size = base::list(
    trees = base::list(
      Title  = "Basal area (m\u00B2) / hectare",
      Cuts   = base::c(-1, 0, 0.5, 1.5, 7.5, 15, 30, 45, 10000000),
      Labels = base::c("0", ">0–0.5", ">0.5–1.5", ">1.5–7.5", ">7.5–15", ">15–30", ">30–45", "45+")),
    saplings = base::list(
      Title  = "Basal Area (m\u00B2) / hectare",
      Cuts   = base::c(-1, 0, 0.25, 0.5, 1.0, 2.0, 3.0, 4.0, 10000000),
      Labels = base::c("0", ">0–0.25", ">0.25–0.5", ">0.5–1.0", ">1.0–2.0", ">2.0–3.0", ">3.0–4.0", "4.0+")),
    herbs = base::list(
      Title  = "Percent Cover",
      Cuts   = base::c(-1, 0, 1, 5, 10, 25, 50, 75, 1000),
      Labels = base::c("0%", ">0–1%", ">1–5%", ">5–10%", ">10–25%", ">25–50%", ">50–75%", "75%+")),
    cwd = base::list(
      Title  = "Volume m3 / hectare",
      Cuts   = base::c(-1, 0, 0.5, 1.5, 7.5, 15, 30, 45, 10000000),
      Labels = base::c("0", ">0–0.5", ">0.5–1.5", ">1.5–7.5", ">7.5–15", ">15–30", ">30–45", "45+")))
)

