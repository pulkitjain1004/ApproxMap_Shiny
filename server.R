library(shiny)
library(shinydashboard)
library(devtools)
install_github("ilangurudev/ApproxMapSeq")
library(ApproxMapSeq)
library(tidyverse)


source("./Helpers/Helpers.R")

#data_uploaded = read.csv("./data/demo1.csv")

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
  
  output$contents <- renderTable({
    if(is.null(data_uploaded())) {
      return("Upload data to view here")
    } else {
      return(head(data_uploaded()))
    }
    })
  
  output$processedOut <- renderPrint(
    {
      if(!is.null(data_uploaded()))
        return(str(data_uploaded()))
    }
  )
  
  ProcessInpAndGetApproxMap = function() {
    if(is.null(data_uploaded())) {
      return(NULL)
    } else {
      inp = cvt_seq(data_uploaded(),pd = 1)
      results = get_approxMap(inp,input$numKNN, input$cons_cutoff)
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
                 wellPanel(
                 tags$h3("Weighted Sequence:"), tags$br(),
                 tags$h4(textOutput(outputId = paste(x,"_wseq",sep=""))),tags$br()),
                 wellPanel(
                 tags$h3("Frequency Plot:"), tags$br(),
                 #numericInput(inputId = paste0(x,"_noise_cutoff"), label = "Noise Cutoff", value = 0),
                 #sliderInput(inputId = paste0(x,"_var_slid"), label = "Select threshold for variation pattern", min = 0, max = input$slidCutoff, value = 0.5 *input$slidCutoff),
                 plotOutput(outputId = paste(x,"_plot",sep="")), tags$br()),
                 wellPanel(
                 tags$h3("Consensus Pattern:"), tags$br(),
                 tags$h4(textOutput(outputId = paste(x,"_cons",sep=""))),tags$br())
                 # wellPanel(
                 #   tags$h3("Variation Pattern:"), tags$br(),
                 #   tags$h4(textOutput(outputId = paste(x,"_var",sep=""))),tags$br())
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
    wseq = paste0("Cluster",x,"_wseq")
    plt = paste0("Cluster",x,"_plot")
    cons = paste0("Cluster",x,"_cons")
    output[[wseq]] <- renderPrint(cat(approxmap_obj()$formatted_results$weighted_seq[[x]]))
    output[[plt]] <- renderPlot(plot_frequency(approxmap_obj()$weighted_seqs[[x]],input$cons_cutoff, input$noise_cutoff, input$var_cutoff))
    output[[cons]] <- renderPrint(cat(approxmap_obj()$formatted_results$consensus[[x]]))
  })
  }
  )
  
  #,input$slidCutoff, output[[paste0("Cluster",x,"_noise_cutoff")]],output[[paste0("Cluster",x,"_var_slid")]]
   
}
     