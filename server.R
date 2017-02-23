library(shiny)
library(shinydashboard)
source("./Helpers/1_Preprocessing.R")
source("./Helpers/2_calculate_dist_and_cost.R")
source("./Helpers/3_alignments.R")
source("./Helpers/4_clustering.R")
source("./Helpers/5_ApproxMap.R")


server <- function(input, output, session) {
  
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
    
      Seqs = data_uploaded()
      Seqs = Seqs[,-1]
      inp2 = process_Input(as.character(Seqs$Seqs), id = Seqs$id, Itemset_regex  = "[\\{*}]")
      results <- get_approxMap(inp2, input$numKNN, input$slidCutoff)
      return(results)
    
  }
  
  approxMap <- eventReactive(input$but_AppMap,ProcessInpAndGetApproxMap())
                             
  observeEvent(input$but_AppMap, {
    updateTabsetPanel(session, "tabs", "Consensus Patterns")
  }
  )
  
  output$approxMapInfo = renderPrint(approxMap())
  
}


