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
library(tensorflow)
library(keras)
library(reticulate)

options(shiny.maxRequestSize = 100 * 1024^2)

architecture <<- matrix(ncol = 2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Variables
  architectureText <- reactiveValues(text = "Input your architecture, layer by layer!")
  lossPlot <- reactiveValues(ll1 = ggplot() + ggtitle("Run Your Model To Get A Loss Plot!") + theme(plot.title = element_text(size = rel(2.5))), 
                             ll2 = ggplot() + ggtitle("Run Your Model To Get A Loss Plot!") + theme(plot.title = element_text(size = rel(2.5))),
                             loss1 = ggplot() + ggtitle("Run Your Model To Get A Loss Plot!") + theme(plot.title = element_text(size = rel(2.5))),
                             loss2 = ggplot() + ggtitle("Run Your Model To Get A Loss Plot!") + theme(plot.title = element_text(size = rel(2.5))))
  
  reticulate::virtualenv_create(envname = 'r-reticulate', python = 'python')
  reticulate::virtualenv_install('r-reticulate', c('numpy', 'tensorflow', 'keras'), ignore_installed = FALSE)
  reticulate::use_virtualenv(virtualenv = 'r-reticulate', required = TRUE)
  
  # Helper Functions
  importData <- function(data1, results1, data2, results2) {
    tryCatch({
      data1 <<- read_csv(data1)
    }, 
    error = function(e) {
      stop(safeError(e))
    })
    
    tryCatch({
      data2 <<- read_csv(data2)
    }, 
    error = function(e) {
      stop(safeError(e))
    })
    
    tryCatch({
      results1 <<- read_csv(results1)
    }, 
    error = function(e) {
      stop(safeError(e))
    })
    
    tryCatch({
      results2 <<- read_csv(results2)
    }, 
    error = function(e) {
      stop(safeError(e))
    })
  }
  
  runNN <- function(globalIterations, localIterations) {
    data1 <<- data1 %>% mutate_all(~replace(., is.nan(.), 0))
    data2 <<- data2 %>% mutate_all(~replace(., is.nan(.), 0))
    
    data1mat <- as.matrix(sapply(data1, as.numeric))
    data2mat <- as.matrix(sapply(data2, as.numeric))
    results1mat <- as.matrix(sapply(results1, as.numeric))
    results2mat <- as.matrix(sapply(results2, as.numeric))
    
    model1 <- keras_model_sequential()
    model2 <- keras_model_sequential()
    for (i in 1:nrow(architecture)) {
      if (i == 1) {
        if (architecture[i, 2] == 1) {
          model1 %>% layer_dense(units = architecture[i, 1], activation = "relu", input_shape = c(dim(data1)[2]))
          model2 %>% layer_dense(units = architecture[i, 1], activation = "relu", input_shape = c(dim(data1)[2]))
        } else {
          model1 %>% layer_dense(units = architecture[i, 1], activation = "sigmoid", input_shape = c(dim(data1)[2]))
          model2 %>% layer_dense(units = architecture[i, 1], activation = "sigmoid", input_shape = c(dim(data1)[2]))
        }
      } else {
        if (architecture[i, 2] == 1) {
          model1 %>% layer_dense(units = architecture[i, 1], activation = "relu")
          model2 %>% layer_dense(units = architecture[i, 1], activation = "relu")
        } else {
          model1 %>% layer_dense(units = architecture[i, 1], activation = "sigmoid")
          model2 %>% layer_dense(units = architecture[i, 1], activation = "sigmoid")
        }
      }
    }
    
    model1 %>% 
      compile(
        loss = "binary_crossentropy", 
        optimizer = "SGD" 
      )
    model2 %>% 
      compile(
        loss = "binary_crossentropy", 
        optimizer = "SGD" 
      )
    w <<- get_weights(model1)
    set_weights(model2, get_weights(model1))
    
    for (i in 1:globalIterations) {
      history1 <- model1 %>%
        evaluate(
          x = data1mat, y = results1mat, 
          verbose = 0
        )
      
      history2 <- model2 %>%
        evaluate(
          x = data2mat, y = results2mat, 
          verbose = 0
        )
      
      ht1 <- model1 %>%
        fit(
          x = data1mat, y = results1mat,
          epochs = localIterations,
          validation_split = 0, 
          verbose = 0
        )

      ht2 <- model2 %>%
        fit (
          x = data2mat, y = results2mat,
          epochs = localIterations,
          validation_split = 0, 
          verbose = 0
        )
      
      if (i == 1) {
        h1 <- as.data.frame(history1)
        h2 <- as.data.frame(history2)
        
        hl1 <- as.data.frame(ht1)
        hl2 <- as.data.frame(ht2)
      } else {
        h1 <- rbind(h1, as.data.frame(history1))
        h2 <- rbind(h2, as.data.frame(history2))
        
        hl1 <- rbind(hl1, as.data.frame(ht1))
        hl2 <- rbind(hl2, as.data.frame(ht2))
      }

      weights1 <- get_weights(model1)
      weights2 <- get_weights(model2)
      for (i in 1:length(weights1)) {
        if (i %% 2 == 0) {
          weight1 <- as.array(weights1[[i]]) * (nrow(data1mat) / (nrow(data1mat) + nrow(data2mat)))
          weight2 <- as.array(weights2[[i]]) * (nrow(data2mat) / (nrow(data1mat) + nrow(data2mat)))
          tempList = list(weight1, weight2)
          w[[i]] <<- Reduce("+", tempList)
        } else {
          weight1 <- as.matrix(weights1[[i]]) * (nrow(data1mat) / (nrow(data1mat) + nrow(data2mat)))
          weight2 <- as.matrix(weights2[[i]]) * (nrow(data2mat) / (nrow(data1mat) + nrow(data2mat)))
          tempList  = list(weight1, weight2)
          w[[i]] <<- Reduce("+", tempList)
        }
      }

      set_weights(model1, w)
      set_weights(model2, w)
    }
    
    hist1 <<- h1
    hist2 <<- h2
    histloss1 <<- hl1
    histloss2 <<- hl2
  }
  
  generateGraphs <- function() {
    hist1$id <<- 1:nrow(hist1)
    hist2$id <<- 1:nrow(hist2)
    histloss1$id <<- 1:nrow(histloss1)
    histloss2$id <<- 1:nrow(histloss2)
    
    l1 <- ggplot(data = hist1, aes(id, history1)) + geom_line(color = "red") + scale_y_continuous(trans = 'log10')
    l2 <- ggplot(data = hist2, aes(id, history2)) + geom_line(color = "red") + scale_y_continuous(trans = 'log10')
    loss1 <- ggplot(data = histloss1, aes(id, value)) + geom_line(color = "red") + scale_y_continuous(trans = 'log10')
    loss2 <- ggplot(data = histloss2, aes(id, value)) + geom_line(color = "red") + scale_y_continuous(trans = 'log10')
    
    loss1 <- loss1 + ggtitle("Training Loss: Server 1") + xlab("Total Epochs") + ylab("Loss")
    loss2 <- loss2 + ggtitle("Training Loss: Server 2") + xlab("Total Epochs") + ylab("Loss")
    
    l1 <- l1 + ggtitle("Federated Loss: Server 1") + xlab("Federated Epochs") + ylab("Loss")
    l2 <- l2 + ggtitle("Federated Loss: Server 2") + xlab("Federated Epochs") + ylab("Loss")
    
    loss1 <- loss1 + theme(plot.title = element_text(size = rel(2.5))) + theme(axis.title.x = element_text(size = rel(1.75))) + theme(axis.title.y = element_text(size = rel(1.75)))
    loss2 <- loss2 + theme(plot.title = element_text(size = rel(2.5))) + theme(axis.title.x = element_text(size = rel(1.75))) + theme(axis.title.y = element_text(size = rel(1.75)))
    
    l1 <- l1 + theme(plot.title = element_text(size = rel(2.5))) + theme(axis.title.x = element_text(size = rel(1.75))) + theme(axis.title.y = element_text(size = rel(1.75)))
    l2 <- l2 + theme(plot.title = element_text(size = rel(2.5))) + theme(axis.title.x = element_text(size = rel(1.75))) + theme(axis.title.y = element_text(size = rel(1.75)))
    lossPlot$ll1 <- l1
    lossPlot$ll2 <- l2
    lossPlot$loss1 <- loss1
    lossPlot$loss2 <- loss2
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
      architectureText$text <- "Input your architecture, layer by layer!"
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
                 input$dataFile2$datapath, input$resultsFile2$datapath)
    }, error = function(e) {
      stop(safeError(e))
    })
    
    tryCatch({
      runNN(input$globalIterations, input$localIterations)
    }, error = function(e) {
      stop(safeError(e))
    })
    
    tryCatch({
      generateGraphs()
    }, error = function(e) {
      stop(safeError (e))
  })
  })
  
  # Output Functions
  output$currentArchitecture <- renderText({
    return(architectureText$text)
  })
  
  output$running <- renderText({
    return(running$text)
  })
  
  output$loss1 <- renderPlot({
    return(lossPlot$loss1)
  })
  
  output$loss2 <- renderPlot({
    return(lossPlot$loss2)
  })
  
  output$ll1 <- renderPlot({
    return(lossPlot$ll1)
  })
  
  output$ll2 <- renderPlot({
    return(lossPlot$ll2)
  })
})
