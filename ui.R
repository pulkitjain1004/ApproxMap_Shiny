library(shiny)

ui <- fluidPage(
  theme = "bootstrap2.css",
  tags$h1("ApproxMap"),
  fluidRow(
    column(4, tags$h2("Steps")),
    column(8, tags$h2("Results"))
  ),
  sidebarLayout(
    sidebarPanel(
      
      wellPanel(
      fileInput(inputId = "inp_data",label = "1. Upload File")
      ),
      
      wellPanel(
        numericInput(inputId = "numKNN", label = "2. Enter number of nearest neighbours", value=1)  
      ),
      
      wellPanel(
        sliderInput(inputId = "slidCutoff", label = "3. Select cutoff", min = 0, max = 1,value = 0.5)
      ),
      
      wellPanel(
        tags$strong("4. Get ApproxMap"),
        tags$br(),
        actionButton(inputId = "but_AppMap", label = "Calculate")
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(id = "tabs",
        tabPanel("Data",
                 tableOutput("contents"),
                 verbatimTextOutput(outputId = "processedOut")
                 ),
        
        tabPanel("Clusters"
                 #verbatimTextOutput(outputId = "ClusterInfo")
                 ),
        
        tabPanel("Consensus Patterns",
                 verbatimTextOutput(outputId = "approxMapInfo")
        )
        
        
      )
      
    )
  )
)
