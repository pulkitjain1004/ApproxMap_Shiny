library(shiny)
library(shinydashboard)
library(devtools)
install_github("ilangurudev/ApproxMapSeq")
library(ApproxMapSeq)
library(tidyverse)


source("./Helpers/Helpers.R")

data_uploaded = read.csv("./data/demo1.csv")

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
      #if(is.null(inp)) return(read.csv("./data/demo1.csv")) #for demo, otherwise NULL
      if(is.null(inp)) return(NULL)
      else return(read.csv(inp$datapath))

    }
  )
  
  output$contents <- renderTable(head(data_uploaded()))
  
  output$processedOut <- renderPrint(
    {
      if(is.null(data_uploaded()))
        return(NULL)
      else
        return(str(data_uploaded()))
    }
  )
  
  ProcessInpAndGetApproxMap = function() {
    if(is.null(data_uploaded())) {
      return(NULL)
    } else {
      inp = cvt_seq(data_uploaded(),pd = 1)
      results = get_approxMap(inp,input$numKNN, input$slidCutoff)
      # format_output(results)
      return(results) 
    }
  }
  
  approxmap_obj <- eventReactive(input$but_AppMap,ProcessInpAndGetApproxMap())
  
  
  #to move to the consensus pattern tab                           
  observeEvent(input$but_AppMap, {
    updateTabsetPanel(session, "tabs", "Consensus Patterns")
  }
  )
  
  #output$approxMapInfo = renderPrint(approxmap_obj())
  rend_cons = function() {
    if(is.null(approxmap_obj())) {
      return("Approxmap not calculated yet")
    } else {
      nTabs = length(approxmap_obj()$clusters) 
      #tabs = c("Overview",paste("Cluster", as.character(1:nTabs)))
      tabs = sapply(1:nTabs, function(x) paste0("Cluster",as.character(x)))
      newTabs = lapply(tabs, function(x) {
        tabPanel(title = x,
                 tags$h3(textOutput(outputId = paste(x,"_info",sep=""))),
                 plotOutput(outputId = paste(x,"_plot",sep=""))
                 #textInput(inputId = "xxx",value = "xxx",label="xxx")
        )
      }
      )
      do.call(tabsetPanel, newTabs)
    }
  }
  
  output$approxMapInfo = renderUI(rend_cons())
  
  observe({
  nTabs = length(approxmap_obj()$clusters) 
  lapply(1:nTabs,function(x) {
    inf = paste0("Cluster",x,"_info")
    plt = paste0("Cluster",x,"_plot")
    output[[inf]] <- renderPrint(cat(approxmap_obj()$formatted_results$weighted_seq[[x]]))
    output[[plt]] <- renderPlot(plot_frequency(approxmap_obj()$weighted_seqs[[x]],input$slidCutoff))
  })
  }
  )
  
  
   
}
     