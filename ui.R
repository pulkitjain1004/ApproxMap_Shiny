library(shiny)
library(lubridate)

ui <- fluidPage(
  theme = "bootstrap3.css",
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
       
        #dateInput(inputId = "st.date",label = "Enter Starting Date for Data",format = "mm-dd-yyyy", startview = "month", weekstart = 0),
        
        
        radioButtons(inputId = "period", 
                     choices =  c("Predefined time periods" ,"Specify manually"), 
                     label = "Select Time Period for Data (To be discussed Later)",
                     selected = "Predefined"
                     ),
        
        
        selectInput("period1", label = "Predefined Time Periods:",
                    #list(`Days` = c("1-d","5-d","10-d"), `Weeks` = c("1-w","2-w","4-w"), `Month` = c("Calender Month","3 Month"))
                     c("1 week","Calender Month", "Quarterly", "6 Month", "1 Year")
        ),
        
    #    numericInput(inputId = "period2", label = "Specify manually (in days) (To be discussed Later):", value=0),
        
        conditionalPanel(
          condition = "input.period1  == '1 week' ",
            selectInput(
              inputId = "days", label = "Day to start from",
              c("Mon","Tue","Wed","Thur","Fri","Sat","Sun"))
        ),
         
        conditionalPanel(
          condition = "input.period1  == 'Calender Month' ",
            textOutput("text1")
       ), 
    
        conditionalPanel(
          condition = "input.period1  == 'Quarterly' ",
            textOutput("text2")
       ), 
    
        conditionalPanel(
          condition = "input.period1  == '6 Month' ",
            selectInput(
              inputId = "days", label = "Month to start from",
              c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec"))
       ),
       
        conditionalPanel(
          condition = "input.period1  == '1 Year' ",
            selectInput(
              inputId = "days", label = "Month to start from",
              c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec"))
    )
    
      ),
      
      wellPanel(
        numericInput(inputId = "numKNN", label = "2. Enter number of nearest neighbours", value=2)
      ),
      
      wellPanel(
        sliderInput(inputId = "slidCutoff", label = "3. Select cutoff", min = 0, max = 1,value = 0.4)
      ),
     
     
     
     
     wellPanel(
        tags$strong("4. Get ApproxMap"),
        tags$br(),
        actionButton(inputId = "but_AppMap", label = "Calculate")
        #actionButton(inputId = "but_Display", label = "Display")
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
                 uiOutput(outputId = "approxMapInfo")
        )
        
        
      )
      
    )
  )
)
