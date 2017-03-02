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
      n = unlist(lapply(approxmap_obj()$weighted_seqs, function(wseq) wseq$n))
      newTabs = lapply(1:nTabs, function(x) {
        tabPanel(title = paste0("Cluster ",x," (n=",n[x],")"),
                 tags$h4(paste("   Number of sequences in cluster = ", n[x])),
                 wellPanel(id = "results_panels",
                   tags$h3("Patterns:"), tags$br(),
                   tags$h4("Consensus Pattern: "),tags$h4(htmlOutput(outputId = paste(tabs[x],"_cons",sep=""))),tags$br(),
                   tags$h4("Variation Pattern: "),tags$h4(htmlOutput(outputId = paste(tabs[x],"_var",sep=""))),tags$br()
                   ),
                 wellPanel(id = "results_panels",
                   tags$h3("Frequency Plot:"), tags$br(),
                   plotOutput(outputId = paste(tabs[x],"_plot",sep="")), tags$br()),
                 wellPanel(id = "results_panels",
                   tags$h3("Weighted Sequence:"), tags$br(),
                   tags$h4(htmlOutput(outputId = paste(tabs[x],"_wseq",sep=""))),tags$br())
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
    # wseq = paste0("Cluster",x,"_wseq")
    # plt = paste0("Cluster",x,"_plot")
    # cons = paste0("Cluster",x,"_cons")
    #output[[wseq]] <- renderPrint(cat(approxmap_obj()$formatted_results$weighted_seq[[x]]))
    output[[paste0("Cluster",x,"_wseq")]] <- renderPrint(cat(get_Wseq_Formatted_HTML(approxmap_obj()$weighted_seqs[[x]])))
    output[[paste0("Cluster",x,"_plot")]] <- renderPlot(plot_frequency(approxmap_obj()$weighted_seqs[[x]],input$cons_cutoff, input$noise_cutoff, input$var_cutoff))
    #output[[paste0("Cluster",x,"_cons")]] <- renderPrint(cat(approxmap_obj()$formatted_results$consensus[[x]]))
    output[[paste0("Cluster",x,"_cons")]] <- renderPrint(cat(get_consensus_formatted_HTML(approxmap_obj()$weighted_seqs[[x]],input$cons_cutoff)))
    #var_pat = get_consensus_formatted(get_consensus_pattern(approxmap_obj()$weighted_seqs[[x]],input$var_cutoff))
    output[[paste0("Cluster",x,"_var")]] <- renderPrint(cat(get_consensus_formatted_HTML(approxmap_obj()$weighted_seqs[[x]],input$var_cutoff)))
  })
  }
  )
  
  observeEvent(input$cons_cutoff,{
    if(!is.null(approxmap_obj())) {
      nTabs = length(approxmap_obj()$clusters) 
      lapply(1:nTabs,function(x) {
        cons = paste0("Cluster",x,"_cons")
        #cons_pat <- get_consensus_pattern(approxmap_obj()$weighted_seqs[[x]],input$cons_cutoff)
        output[[cons]] <- renderPrint(cat(get_consensus_formatted_HTML(approxmap_obj()$weighted_seqs[[x]],input$cons_cutoff)))
      })
       
    }
  })
  
  observeEvent(input$var_cutoff,{
    if(!is.null(approxmap_obj())) {
      nTabs = length(approxmap_obj()$clusters)
      lapply(1:nTabs,function(x) {
        var = paste0("Cluster",x,"_var")
        var_pat <- get_consensus_pattern(approxmap_obj()$weighted_seqs[[x]],input$var_cutoff)
        output[[var]] <- renderPrint(cat(get_consensus_formatted_HTML(approxmap_obj()$weighted_seqs[[x]],input$var_cutoff)))
      })

    }
  })
  
  observe({
    updateSliderInput(session,inputId = "cons_cutoff", min = input$var_cutoff, max = 1, value = (1 + input$var_cutoff)/2)
  })
  
  #output$test <- renderText(paste("<font color=\"red\" size=10>This is some text!</font>"))
  output$test <- renderText(paste("<priority1>This is some text!</priority1>"))
  #priority1
}
     