library(shiny)
library(dplyr)
library(tidytext)
library(ggplot2)
library(DT)
library(tidyverse)
library(shinyalert)

# User interface
ui <- fluidPage(
  useShinyalert(),
  navbarPage("EHR EDA App",
  
    # Merged & Cleaned
    tabPanel(
      "Merged",
      fluid = TRUE,
      
      sidebarLayout(
        sidebarPanel(width = 12,
                     p("Choose CSV file with 'Merged' EHR data."),
                     fileInput("merged_input", "Load data", accept = c(".csv")),
                     selectInput("merged_specialty_selector", "Filter by Medical Specialty:",
                                 choices = c("All" = "all",
                                   "Cardiology" = "card",
                                   "Detoxification" = "tox",
                                   "Digestive" = "dige",
                                   "Endocrine" = "endo",
                                   "Hematology" = "hema",
                                   "Infectious Diseases" = "infe",
                                   "Internal Medicine" = "inte",
                                   "Nefrology" = "nefr",
                                   "Neumology" = "neumo",
                                   "Neurology" = "neuro",
                                   "Oncology" = "onco",
                                   "Psychiatry" = "psiq",
                                   "Dermatology" = "derm"
                                   ),
                                 multiple = F)
                     
        ), # sidebarPanel 
        
        mainPanel(width = 12,
                  h2(textOutput("merged_explorer_title")),
                  dataTableOutput("merged_table"),
                  h2(textOutput("merged_stat_info_title")),
                  verbatimTextOutput("merged_stat_info"),
                  h2(textOutput('numWords_title')),
                  conditionalPanel(
                    condition = "output.numWordsPlotShown == true",
                    sliderInput("numWords_slider", "Number of top words:", min = 1, max = 30, value = 10)
                  ),
                  column(width = 12,
                         plotOutput('numWords_plot')
                  )
                  
          
        ) # mainPanel
      ) #sidebarLayout
    ), # tabPanel
    
    
    # OneHot & ReducedOneHot
    tabPanel(
      "OneHot",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(width = 12,
                     p("Choose CSV file with 'OneHot' EHR data."),
                     fileInput("onehot_input", "Load data", accept = c(".csv")),
                     selectInput("onehot_specialty_selector", "Filter by Medical Specialty:",
                                 choices = c("All" = "all",
                                             "Cardiology" = "card",
                                             "Detoxification" = "tox",
                                             "Digestive" = "dige",
                                             "Endocrine" = "endo",
                                             "Hematology" = "hema",
                                             "Infectious Diseases" = "infe",
                                             "Internal Medicine" = "inte",
                                             "Nefrology" = "nefr",
                                             "Neumology" = "neumo",
                                             "Neurology" = "neuro",
                                             "Oncology" = "onco",
                                             "Psychiatry" = "psiq",
                                             "Dermatology" = "derm"
                                 ),
                                 multiple = F),
                     selectInput("icd_selector", "Filter by ICD code:",
                                 choices = c("All" = "all"),
                                 multiple = F)
                     
        ), # sidebarPanel
        mainPanel(width = 12,
                  h2(textOutput("onehot_stat_info_title")),
                  verbatimTextOutput("onehot_stat_info"),
                  h2(textOutput('icdAnalysis_title')),
                  conditionalPanel(
                    condition = "output.icdDistPlotShown == true",
                    h4(textOutput('reduceSimulator_title')),
                    splitLayout(cellWidths = c("20%", "20%"),
                                sliderInput("reduceSimulator_slider", "Minimum % of documents", min = 0.1, max = 100.0, value = 5.0, step=0.1),
                                verbatimTextOutput("reduceSimulator_output"),
                                tags$style(type='text/css', "#reduceSimulator_output { width:100%; margin-top: 35px;}")
                    ),
                    h4(textOutput('rangeICDs_title')),
                    splitLayout(cellWidths = c("20%", "20%"),
                                numericInput("rangeICDs_min", "Start position:", 1, min = 1, max = 30, step = 1),
                                numericInput("rangeICDs_max", "End position:", 30, min = 1, max = 30, step = 1)
                    ),
                    #sliderInput("numICDs_slider", "Range of ICDs:", min = 0, max = 1, value = c(0,1), step = 1)
                  ),
                  splitLayout(cellWidths = c("50%", "50%"),
                              plotOutput("icdDist_plot"),
                              plotOutput("icdHist_plot")
                  ),
                  
        )
      ) # sidebarLayout
    )
  )
)
  
  
# Server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize=500*1024^2)
  
  # CONDITIONAL UI
  output$numWordsPlotShown <- reactive({
    return(!is.null(getNumWordsPlot()))
  })
  outputOptions(output, 'numWordsPlotShown', suspendWhenHidden=FALSE)
  
  output$icdDistPlotShown <- reactive({
    return(!is.null(getICDDistPlot()))
  })
  outputOptions(output, 'icdDistPlotShown', suspendWhenHidden=FALSE)
  
  # REACTIVE FUNCTIONS
  
  # Read files
  getMerged <- reactive({
    req(input$merged_input$datapath)
    withProgress(message = "Reading dataset...", {
      dfMerged <- read.csv(input$merged_input$datapath, stringsAsFactors = FALSE)
      incProgress(0.8)
      if(input$merged_specialty_selector != 'all'){
        dfMergedFiltered <- dfMerged %>% dplyr::filter(str_detect(tolower(HADM_ID), input$merged_specialty_selector))
        if(nrow(dfMergedFiltered) > 0){
          dfMerged <- dfMergedFiltered
        }
        else{
          #showNotification("No reports of the selected specialty found, removing filter...")
          updateSelectInput(session, "merged_specialty_selector", selected = "all")
          shinyalert("No report found", paste("No reports of ", input$merged_specialty_selector, " found, removing filter..."), type = "error")
        }
      }
      
      incProgress(1.0)
      return(dfMerged)
      
    })
  })
  
  getOneHot <- reactive({
    req(input$onehot_input$datapath)
    withProgress(message = "Reading dataset...", {
      dfOneHot <- read.csv(input$onehot_input$datapath, stringsAsFactors = FALSE)
      incProgress(0.8)
      labels = colnames(dfOneHot)[3:length(colnames(dfOneHot))]
      updateSelectInput(session, "icd_selector", choices = append(c("All"), labels))
      if(input$onehot_specialty_selector != 'all'){
        dfOneHotFiltered <- dfOneHot %>% dplyr::filter(str_detect(tolower(HADM_ID), input$onehot_specialty_selector))
        if(nrow(dfOneHotFiltered) > 0){
          dfOneHot <- dfOneHotFiltered
        }
        else{
          updateSelectInput(session, "onehot_specialty_selector", selected = "all")
          shinyalert("No report found", paste("No reports of ", input$onehot_specialty_selector, " found, removing filter..."), type = "error")
        }
      }
      incProgress(1.0)
      return(dfOneHot)
    })
  })
  
  # Get Tidy Dataframe
  getTidyDf <- reactive({
    withProgress(message = "Computing Tidy Dataframe", {
    dfMerged <- getMerged()
    text_df <- tibble(line = 1:nrow(dfMerged), text = dfMerged$TEXT)
    incProgress(0.5)
    tidy_text <- text_df %>% unnest_tokens(word, text)
    incProgress(1.0)
    })
    return(tidy_text)
  })
  
  # Get Labels
  getLabelCount <- reactive({
    dfOneHot <- getOneHot()
    label_count <- sort(sapply(dfOneHot[, 3:ncol(dfOneHot)], sum), decreasing = T)
    label_count <- label_count[ label_count != 0 ]
    
    #updateSliderInput(session, "numICDs_slider", min = 1, max = length(label_count), value = c(1, min(length(label_count), 30)))
    updateNumericInput(session, "rangeICDs_min", value = 1, min = 1, max = length(label_count), step = 1)
    updateNumericInput(session, "rangeICDs_max", value = min(length(label_count), 30), min = 1, max = length(label_count), step = 1)
    df_label_count <- data.frame(label=names(label_count), count=label_count, row.names = NULL)
    df_label_count$label <- factor(df_label_count$label, levels = df_label_count$label[order(df_label_count$count, decreasing = T)])
    return(df_label_count)
    
  })
  
  
  # Get Plots
  getNumWordsPlot <- reactive({

    withProgress(message = "Computing top words", {
      
      dfMerged <- getMerged()
      incProgress(0.25)
      tidy_df <- getTidyDf()
      incProgress(0.50)
      
      # Stop words
      custom_stop_words <- bind_rows(stop_words, tibble(word = tm::stopwords("spanish"),lexicon = "custom"))
      
      plot_out <- tidy_df %>%
        dplyr::filter(is.na(as.numeric(word))) %>%
        anti_join(custom_stop_words) %>%
        count(word, sort = TRUE) %>%
        slice(1:input$numWords_slider) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) + geom_col() + xlab("word") + ylab("number of ocurrences") +
        theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + coord_flip()
      
      incProgress(1.0)
      
      return(plot_out)
      
    })
    
    
  })
  
  getICDDistPlot <- reactive({
    
    withProgress(message = "Computing ICD distribution...", {
      
      df_label_count <- getLabelCount()
      incProgress(0.8)
      plot_out <- ggplot(df_label_count[input$rangeICDs_min:input$rangeICDs_max,], aes(label, count)) + geom_col() +
        theme(plot.title = element_text(size=16), axis.text=element_text(size=12), axis.title=element_text(size=14)) +
        labs(title="Label distribution", x = "icd code", y="number of documents")
        
      incProgress(1.0)
      return(plot_out)
    })
  })
  
  getICDHistPlot <- reactive({
    
    withProgress(message = "Computing ICD histogram...", {
      
      df_label_count <- getLabelCount()
      label_density <- density(df_label_count$count)
      
      Q <- quantile(df_label_count$count, c(0.0, 0.25, 0.5, 0.75, 0.8, 0.9, 1.0))
      
      incProgress(0.8)
      
      plot_out <- ggplot(df_label_count, aes(df_label_count$count)) + 
        geom_histogram(color="darkblue", fill="lightblue", bins=nclass.Sturges(df_label_count$count), boundary=0) + scale_x_log10() + 
        geom_vline(xintercept=Q[2], linetype='dashed', colour='forestgreen') + 
        annotate("text", x = Q[2], y = 0, angle = 0, label = "Q1", vjust = 1.5) + 
        geom_vline(xintercept=Q[3], linetype='dashed', colour='orangered2') + 
        annotate("text", x = Q[3], y = 0, angle = 0, label = "Q2", vjust = 1.5) + 
        geom_vline(xintercept=Q[4], linetype='dashed', colour='deeppink4') + 
        annotate("text", x = Q[4], y = 0, angle = 0, label = "Q3", vjust = 1.5) + 
        theme(plot.title = element_text(size=16), axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
        labs(title="Label histogram", x = "number of documents", y="icd count")
      incProgress(1.0)
      
      return(plot_out)
    })
  })
  
  output$merged_table <- renderDataTable({
    DT::datatable(getMerged(), options = list(pageLength=1, searching=F,
                                              columnDefs = list(list(targets = 2,
                                                                     render = DT::JS(
                                                                       "function(data, type, row, meta) {",
                                                                       "return type === 'display' && data != null && data.length > 280 ?",
                                                                       "'<span title=\"' + data + '\">' + data.substr(0, 280) + '...</span>' : data;",
                                                                       "}")
                                                                     ),
                                                                list(targets = 3,
                                                                     render = DT::JS(
                                                                       "function(data, type, row, meta) {",
                                                                       "return type === 'display' && data != null && data.length > 20 ?",
                                                                       "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                                       "}")
                                                                )
                                                                )
    ), class = "display")
  })
  
  output$merged_stat_info <- renderText({

    dfMerged <- getMerged()
    num_samples <- nrow(dfMerged)
    tidy_df <- getTidyDf()

    words_count <- tidy_df %>% count(word, sort = TRUE)

    out = c("Number of samples: ", num_samples)
    out <- append(out, c("\nNumber of words:", nrow(tidy_df)))
    out <- append(out, c("\nVocabulary size:", nrow(words_count)))
    out <- append(out, c("\nWords per doc:", '???'))

    return(out)

  })
  
  output$reduceSimulator_output <- renderText({
    
    dfOneHot <- getOneHot()
    df_label_count <- getLabelCount()
    

    min <- floor(nrow(dfOneHot) * input$reduceSimulator_slider/100)
    labels_to_keep <- nrow(df_label_count[df_label_count$count > min,])
    
    out <- c("Number of labels kept: ", labels_to_keep)
    return(out)
    
  })
  
  output$onehot_stat_info <- renderText({
    
    dfOneHot <- getOneHot()
    df_label_count <- getLabelCount()
    num_samples <- nrow(dfOneHot)
    avg_card <- mean(rowSums(dfOneHot[,3:ncol(dfOneHot)]))
    
    out = c("Number of samples: ", num_samples)
    out <- append(out, c("\nNumber of labels:", nrow(df_label_count)))
    out <- append(out, c("\nAverage cardinality:", round(avg_card, 2)))
    
    return(out)
    
  })
  
  output$numWords_plot <- renderPlot({
      return(getNumWordsPlot())
  })
  
  output$icdDist_plot <- renderPlot({
    return(getICDDistPlot())
  })
  
  output$icdHist_plot <- renderPlot({
    return(getICDHistPlot())
  })
  
  
  # OBSERVERS TO UPDATE UI
  
  observeEvent(getMerged(), {
    req(input$merged_input$datapath)
    output$merged_explorer_title <- renderText({return("Merged Explorer")})
  })
  
  observeEvent(getTidyDf(), {
    req(input$merged_input$datapath)
    output$merged_stat_info_title <- renderText({return("Statistical information")})
  })
  
  observeEvent(getOneHot(), {
    req(input$onehot_input$datapath)
    output$onehot_stat_info_title <- renderText({return("Statistical information")})
  })

  observeEvent(getNumWordsPlot(), {
    output$numWords_title <- renderText({return("Top Words")})
  })
  
  observeEvent(getICDDistPlot(), {
    output$icdAnalysis_title <- renderText({return("ICD analysis")})
    output$reduceSimulator_title <- renderText({return("Label-set reducer simulator")})
    output$rangeICDs_title <- renderText({return("Select range")})
  })
  
  observeEvent(getICDHistPlot(), {
    #@TODO
  })
  
  observeEvent(input$rangeICDs_max, {
    if(input$rangeICDs_max-30 < 1){
      updateNumericInput(session, "rangeICDs_min", value = 1)
    }else{
      updateNumericInput(session, "rangeICDs_min", value = input$rangeICDs_max-30)
    }
  })
  
  observeEvent(input$rangeICDs_min, {
    df_label_count <- getLabelCount()
    num_labels <- nrow(df_label_count)
    if(input$rangeICDs_min+30 > num_labels){
      updateNumericInput(session, "rangeICDs_max", value = num_labels)
    }else{
      updateNumericInput(session, "rangeICDs_max", value = input$rangeICDs_min+30)
    }
  })
  
  

  
  


    
}
  
shinyApp(ui, server)

