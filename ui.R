#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Binary Prediction Federated Learning Simulator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       fileInput("dataFile1", "Choose Data CSV File 1", 
                 multiple = FALSE, 
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain", 
                            ".csv")), 
       fileInput("resultsFile1", "Choose Results CSV File 1", 
                 multiple = FALSE, 
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain", 
                            ".csv")),
       fileInput("dataFile2", "Choose Data CSV File 2", 
                 multiple = FALSE, 
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain", 
                            ".csv")),
       fileInput("resultsFile2", "Choose Results CSV File 2", 
                 multiple = FALSE, 
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain", 
                            ".csv")),
       tags$hr(),
       
       numericInput("layerSize", label="Layer Size", value = 1),
       radioButtons("activation", "Activation Function", 
                    choices = c(ReLU = "ReLU", 
                                Sigmoid = "Sigmoid"), 
                    selected = "ReLU"), 
       actionButton("delete", label = "Delete"),
       actionButton("add", label = "Add"),
       tags$hr(), 
       
       textOutput("currentArchitecture"), 
       tags$hr(), 
       
       numericInput("localIterations", label = "Local Iterations", value = 10),
       numericInput("globalIterations", label = "Global Iterations", value = 10),
       tags$hr(),
       
       actionButton("run", label = "Run")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("loss1"),
       plotOutput("loss2")
    )
  )
))
