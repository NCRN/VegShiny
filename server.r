library(shiny)
library(NPSForVeg)

NCRN<-importNCRN("T:/I&M/MONITORING/Forest_Vegetation/RCode/VegData")
names(NCRN)<-getNames(NCRN, name.class="code")
Parklist<-getNames(NCRN,name.class="code")
names(Parklist)<-getNames(NCRN)
shinyServer(function(input, output){
  
  output$parkControl<-renderUI({
    selectizeInput(inputId="Park.in",label="Park:", choices=Parklist)
  })   
  
  output$TestIV<-renderPlot({
    print(densplot(NCRN[input$Park.in], densargs=list(group="trees", years=c(2010:2013)), top=5, Total=F))
  })
  
  
})