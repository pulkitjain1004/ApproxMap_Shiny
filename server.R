library(shiny)
library(shinydashboard)
library(devtools)
install_github("ilangurudev/ApproxMapSeq")
library(ApproxMapSeq)

source("./Helpers/Helpers.R")

server <- function(input, output, session) {
  
  output$text1 <- renderText({ 
    "Start day is predefined (First day of every calender month)"
  })
  
  output$text2 <- renderText({ 
    "Start day is predefined (Jan 1, Apr 1, July 1, Oct 1)"
  })
  
  data_uploadedL = isolate(input$inp_dataL)
  
  
  
  data_uploaded = reactive(
    {
      inp <- input$inp_data
      if(is.null(inp)) return(NULL)
      else return(read.csv(inp$datapath))

    }
  )
  
  output$contents <- renderTable(data_uploaded())
  
  # printOutput <- eventReactive(input$but_AppMap, {
  #   
  #   return("Please go to the results tabs to view output")})
  # 
  # output$processedOut <- renderPrint(
  #   {
  #     if(is.null(printOutput()))
  #       return(NULL)
  #     else
  #       return(str(printOutput()))
  #     
  #   })
  # 
   
  output$processedOut <- renderPrint(
    {
      if(is.null(data_uploaded()))
        return(NULL)
      else
        return(str(data_uploaded()))
    }
  )
  
  ProcessInpAndGetApproxMap = function() {
    
    inp = cvt_seq(data_uploaded(),pd = 1)
    results = get_approxMap(inp,input$numKNN, input$slidCutoff)
    format_output(results)
    #return(results$formatted_results)
  }
  
  approxMap <- eventReactive(input$but_AppMap,ProcessInpAndGetApproxMap())
                             
  observeEvent(input$but_AppMap, {
    updateTabsetPanel(session, "tabs", "Consensus Patterns")
  }
  )
  
  output$approxMapInfo = renderPrint(approxMap())
  
}


