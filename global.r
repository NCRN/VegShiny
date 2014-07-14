
BlueOr<-colorRampPalette(c("cyan","magenta4","orangered3")) # colors for circles
AquaYel<-colorRampPalette(c("aquamarine4","green","yellow","goldenrod4")) #colors for polygons
######################### Legend for Map
MapLegend<-list(
  count=list(
    trees=list(
      Title="Trees/ha",
      Cuts=c(-1,0,25,50,100,200,400,600,100000),
      Labels=c("0","0 - 25","25 - 50","50 - 100","100 - 200","200 - 400","400 - 600","600+")
    ),
    saplings=list(
      Title="Saplings/ha",
      Cuts=c(-1,0,150,300,600,1200,1800,2400,100000),
      Labels=c("0","0 - 150", "150 - 300", "300 - 600","600 - 1200","1200 - 1800","1800 - 2400","2400+")
    ),
    seedlings=list(
      Title="Tree seedlings/ha",
      Cuts=c(-1,0,2500,5000,7500,10000,20000,40000,100000000),
      Labels=c("0","0 - 2500","2500 - 5000","5000 - 7500","7500 - 10,000","10,000 - 20,000","20,000 - 40,000","40,000+")
    ),
    shrubs=list(
      Title="Shrubs/ha",
      Cuts=c(-1,0,250,500,1000,1500,3000,6000,100000),
      Labels=c("0","0 - 250","250 - 500","500 - 1000","1000 - 1500", "1500 - 3000", "3000 - 6000", "6000+")
    ),
    shseedlings=list(
      Title="Shrub seedlings/ha",
      Cuts=c(-1,0,2500,5000,7500,10000,20000,40000,100000000),
      Labels=c("0","0 - 2500","2500 - 5000","5000 - 7500","7500 - 10,000","10,000 - 20,000","20,000 - 40,000","40,000+")
    ),
    vines=list(
      Title="Trees with vines/ha",
      Cuts=c(-1,0,25,50,100,250,500,1000,100000),
      Labels=c("0","0 - 25","25 - 50","50 - 100","100 - 250", "250 - 500", " 500 - 1000", "1000+")
    )
  ),
  size=list(   
    trees=list(
      Title="Basal area m2/ha",
      Cuts=c(-1,0,0.5, 1.5, 7.5,15,30,45,10000000),
      Labels=c("0","0 - 0.5","0.5 - 1.5","1.5 - 7.5","7.5 - 15","15 - 30","30 - 45","45+")
    ),
    saplings=list(
      Title="Basal Area m2/ha",
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

