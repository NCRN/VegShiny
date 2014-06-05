

library(shiny)
library(NPSForVeg)

shinyUI(

  fluidPage(
    
    titlePanel(title="Veg Visualizer", windowTitle="Veg Vis"),
  
    fluidRow(
      column(3,
          uiOutput(outputId="ParkControl"),
          sliderInput(inputId="YearIn", label="Display data from the 4 years ending:", min=2009, max=2013, value=2013, format="####"),
          sliderInput(inputId="TopIn", label="Number of species to display (in order of mean value):", min=1, max=10, value=5, format="##"),
         hr(),
         br(),
          radioButtons(inputId="densvalues", label="Type of data to display", 
                       choices=list("Abundance"="count","Size"="size","Occupancy"="presab") ),
         hr(),
         br(),
         tags$div(title="Chose the type of plant you want to work with",
                 radioButtons(inputId="densgroup", label="Type of plant",
                       choices=c("trees","saplings","seedlings","herbs")))
          
    ),

    column(9,

          plotOutput("Testdens")
    )
    )
  
))