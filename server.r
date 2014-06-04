library(shiny)
library(NPSForVeg)

NCRN<-importNCRN("T:/I&M/MONITORING/Forest_Vegetation/RCode/VegData")
names(NCRN)<-getNames(NCRN, name.class="code")
ParkList<-getNames(NCRN,name.class="code")
names(ParkList)<-getNames(NCRN)

shinyServer(function(input, output){
  
  output$ParkControl<-renderUI({expr=
    selectInput(inputId="ParkIn",choices=ParkList, label="Park:") 
    })

     

  output$Testdens<-renderPlot({
   print(densplot(NCRN[input$ParkIn], densargs=list(group="trees", years=c((input$YearIn-4):input$YearIn)), top=5, Total=F))
 })
  
  
})