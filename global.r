
BluePur<-colorRampPalette(c("cyan","darkorchid4"))
MapLegend<-list(
  count=list(
    trees=list(
      Title="Trees/ha",
      Cuts=c(-1,0,25,50,100,100000),
      Labels=c("0","0 - 25","25 - 50","50 - 100","100+")
    ),
    saplings=list(
      Title="Saplings/ha",
      Cuts=c(-1,0,250,500,1000,100000),
      Labels=c("0","0 - 250","250 - 500","500 - 1000","1000+")
    )
  ),
  size=list(
    herbs=list(
      title="Percent Cover",
      Cuts=c(-1,0,25,50,75,101),
      Labels=c("0%","0-25%","25-50%","50-75%","75-100%")
    )
  )
)