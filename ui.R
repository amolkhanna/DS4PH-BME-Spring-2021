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
      h3("Instructions"), 
      p("This is a federated learning simulator designed for binary prediction. 
        The simulator requires 4 CSV files, 2 CSV files containing data, and the remaining 
        two containing the results, all of which should not include row names. 
        2 CSV files must contain your data without any row names, and 2 CSV files must 
        contain your results without any row names. When inputting your desired architecture, 
        the program will infer your input layer based on the data provided. For that reason, 
        you only need to provide layers after the input layer. Finally, you should choose 
        the desired number of global (federated) to run, and the number of local iterations 
        per global iteration. Once you have uploaded your data and inputted the required 
        architecture and iteration counts, the program will run for a total of 
        (global iterations x local iterations) epochs. Please be patient after clicking run; 
        this application initializes and trains multiple neural networks, and can take up 
        to 10 minutes to run.", 
        style = "font-size: 16px;"), 
      tags$hr(), 
      p("The program outputs 4 graphs. 2 of these graphs demonstrate the loss after each 
        local iteration on each client. As a result, they will show the loss for 
        (global iterations x local iterations) epochs. A decreasing loss on these graphs 
        indicates that the model is training locally. The other 2 graphs will demonstrate 
        the federated loss after each global iteration. These will show loss of the overall 
        model after the weights from each local model are averaged. A decreasing loss on 
        these graphs indicates that the federated model indeed improves after the weights 
        from the locally trained models are averaged.", 
        style = "font-size: 16px;"),
      tags$hr(),
      
      plotOutput("loss1"),
      plotOutput("loss2"),
      plotOutput("ll1"),
      plotOutput("ll2"), 
    )
  )
))
