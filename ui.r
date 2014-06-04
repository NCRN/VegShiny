

library(shiny)
library(NPSForVeg)

shinyUI(

  pageWithSidebar(

    headerPanel(title="Veg Visualizer", windowTitle="Veg Vis"),
  
  
  sidebarPanel(

    uiOutput(outputId="ParkControl"),
    
     
    sliderInput(inputId="YearIn", label="Display data from the 4 years ending:", min=2009, max=2013, value=2013, format="####")
    ),

    mainPanel(
          plotOutput("Testdens")
    )
  
))