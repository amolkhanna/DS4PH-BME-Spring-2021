#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

architecture <<- matrix(ncol = 2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Variables
  architectureText <- reactiveValues(text = "Input your architecture, layer by layer!
                                     Make sure to include an input layer!")
  
  # Helper Functions
  importData <- function(data1, results1, data2, results2, header, sep, quote) {
    tryCatch({
      data1 <<- read.csv(data1, 
                         header = header, 
                         sep = sep, 
                         quote = quote)
    }, 
    error = function(e) {
      stop(safeError(e))
    })
    
    tryCatch({
      data2 <<- read.csv(data2,
                         header = header,
                         sep = sep, 
                         quote = quote)
    }, 
    error = function(e) {
      stop(safeError(e))
    })
    
    tryCatch({
      results1 <<- read.csv(results1,
                         header = header,
                         sep = sep, 
                         quote = quote)
    }, 
    error = function(e) {
      stop(safeError(e))
    })
    
    tryCatch({
      results2 <<- read.csv(results2,
                         header = header,
                         sep = sep, 
                         quote = quote)
    }, 
    error = function(e) {
      stop(safeError(e))
    })
    
    print(head(data1))
  }
  
  # Observer Functions
  observeEvent(input$add, {
     validate(
       need(input$layerSize >= 1 && 
            input$layerSize %% 1 == 0, 
            "Need an Integer Layer Size Bigger than 1!")
     )
     
     activator <- 0
     if (input$activation == "ReLU") {
       activator <- 1
     } else {
       activate <- 2
     }
     
     if (is.na(architecture[1, 1])) {
       architecture <<- matrix(c(input$layerSize, activator), ncol = 2)
     } else {
       architecture <<- rbind(architecture, c(input$layerSize, activator))
     }
     
     out <- "Data"
     for(i in 1:(nrow(architecture))) {
       activator <- ""
       if (architecture[i, 2] == 1){
         activator <- "ReLU"
       } else {
         activator <- "Sigmoid"
       }
       
       out <- paste0(out, " :: ", architecture[i, 1], ":", activator)
     }
     
     architectureText$text <- out
   })
  
  observeEvent(input$delete, {
    if (nrow(architecture) == 1) {
      architecture[1,] <<- c(NA, NA)
      architectureText$text <- "Input your architecture, layer by layer! 
      Make sure to include an input layer!"
    } else {
      architecture <<- matrix(architecture[-(nrow(architecture)),], ncol = 2)
      
      out <- "Data"
      for(i in 1:(nrow(architecture))) {
        activator <- ""
        if (architecture[i, 2] == 1){
          activator <- "ReLU"
        } else {
          activator <- "Sigmoid"
        }
        
        out <- paste0(out, " :: ", architecture[i, 1], ":", activator)
      }
      
      architectureText$text <- out
    }
  })
  
  observeEvent(input$run, {
    req(input$dataFile1)
    req(input$resultsFile1)
    req(input$dataFile2)
    req(input$resultsFile2)
    tryCatch({
      importData(input$dataFile1$datapath, input$resultsFile1$datapath, 
                 input$dataFile2$datapath, input$resultsFile2$datapath, 
                 input$header, input$sep, input$quote)
    }, error = function(e) {
      stop(safeError(e))
    })
  })
  
  # Output Functions
  output$currentArchitecture <- renderText({
    return(architectureText$text)
  })
})
