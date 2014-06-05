library(shiny)
library(NPSForVeg)

NCRN<-importNCRN("T:/I&M/MONITORING/Forest_Vegetation/RCode/VegData")
names(NCRN)<-getNames(NCRN, name.class="code")
ParkList<-getNames(NCRN,name.class="code")
names(ParkList)<-getNames(NCRN)

shinyServer(function(input, output){
  
  output$ParkControl<-renderUI({
    selectizeInput(inputId="ParkIn",choices=ParkList, label="Park:",
                   options = list(placeholder='Choose a park',
                           onInitialize = I('function() { this.setValue(""); }') )) 
    })
   
 
  output$Testdens<-renderPlot({
    print(
      if (is.null(input$ParkIn) || nchar(input$ParkIn)==0) {return()}
      else{
        densplot(NCRN[input$ParkIn], densargs=list(group=input$densgroup, years=c((input$YearIn-3):input$YearIn),values=input$densvalues),
                 top=input$TopIn, Total=F)
      }
    )
  })
})