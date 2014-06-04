
# ui file

library(shiny)

shinyUI(fluidPage(
  
  titlePanel(title="Veg Visualizer", windowTitle="Veg Vis"),
  
  
  fluidRow(
    column(3,
      wellPanel(uiOutput("parkControl"))
      ),
    column(9,
           plotOutput("TestIV")
           )
    )
  ))