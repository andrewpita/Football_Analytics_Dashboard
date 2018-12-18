#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

matches = readRDS("matches.rds")
matches_list = matches$Versus

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel("Match HUD"),
  
  selectInput("teams", label = "Select Match", choices = matches_list),
  
  selectInput("GPsp", label = "Plot Type", choices = c("xGP", "xSP")),
  
  selectInput("fullGame", label = "Full Game or Play by Play", choices = c("Full Game",
              "Play by Play")),
  
  textOutput("possession"),
  
  plotOutput("plots"),
  
  hr(),
  
  fluidRow(
    column(1,offset = 1,
           actionButton("Increment","Next Possession"),
           actionButton("Decrement", "Previous Possession")
           )

  )

  )
)

