library(shiny)
library(dentalAffinities)
library(MASS)
library(openxlsx)
library(ggbiplot)
library(shinyjs)
library(tidyverse)
library(readxl)
library(copula)
library(tableHTML)
library(DT)

#source("functions.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Dental Affinities"),
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Descriptives and data preparation",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("descriptivesFile", label = "Please upload a descriptives file here",accept = c(".csv", ".rds", ".xlsx")),
                   selectInput("groupTrait", label = "Grouping for trait frequencies",
                               choices = c("No grouping" = "nogroup", "Group1" = "group 1", "Group2" = "group 2", "Group 1 + Group 2" = "bothgroups")),
                   actionButton("runDescriptives", "Run"),
                   selectInput("corMethod", label = "Correlation method",
                               choices = c("Kendall" = "kendall", "Pearson" = "pearson", "Spearman" = "spearman")),
                   actionButton("traitCorrelation", label = "Check for trait correlation"),
                   hidden(numericInput("corFlag", label = "Flag correlations at > ", value = 0.499)),
                   br(),
                   br(),
                   hidden(downloadButton("downloadDescExcel", "Download Excel")),
                   br(),
                   br(),
                   hidden(downloadButton("downloadDescCSV", "Download CSV"))
                 ),
                 mainPanel = mainPanel(
                   tableOutput("descTable"),
                   dataTableOutput("corTable")
                 )
               )
               
               ),
      tabPanel("Analysis and visualisation",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("descriptivesFile1", label = "Please upload a data file here"),
                   selectInput("method_sel", label = "Method selection",
                               choices = c("MMD - Anscombe" = "MMD_ANS_0",
                                           "MMD - Freeman & Tukey" = "MMD_FRE_0",
                                           "MMD - Anscombe (Freeman & Tukey correction)" = "MMD_ANS_FRE",
                                           "MMD - Anscombe (Grewal correction)" = "MMD_ANS_GRE",
                                           "MMD - Freeman & Tukey (Freeman & Tukey correction)" = "MMD_FRE_FRE",
                                           "MMD - Freeman & Tukey (Grewal correction)" = "MMD_FRE_GRE",
                                           "Mahalanobis - tetrachoric correlation (TMD)" = "MAH_TMD"
                               ),
                               selected = "MMD_ANS"),
                   checkboxInput("minNumberCheck", label = "Minimum number of observations/groups"),
                   hidden(numericInput("minNumber", label = "Min", value = 10)),
                   checkboxInput("remTraitsCheck", label = "Remove traits exhibiting no variation"),
                   hidden(numericInput("remTraits", label = "MD<", value = 0)),
                   checkboxInput("dicData", label = "Dichotomise data"),
                   hidden(selectInput("dicDataInput", label = "Group", choices = c("User" = "user",
                                                                                   "Balanced" = "balanced",
                                                                                   "Chi-Square" = "Chi2"),
                                      selected = "user")),
                   actionButton("runAnalysis", "Run")
                 ),
                 mainPanel = mainPanel(
                  #tableOutput("descTable") 
                 )
               )
               
               ),
      tabPanel("Help",
               )
    )
  )
)
#   fluidRow(class = "myRow1",
#            column(width = 3, h2("Dental Affinities"),
#                   p("Link to the article, contact email"),
#                   checkboxInput("button",label = "Example data", value = TRUE)),
#            column(width = 3,
#                   selectInput("method_sel", label = "Method selection",
#                               choices = c("MMD - Anscombe" = "MMD_ANS_0",
#                                           "MMD - Freeman & Tukey" = "MMD_FRE_0",
#                                           "MMD - Anscombe (Freeman & Tukey correction)" = "MMD_ANS_FRE",
#                                           "MMD - Anscombe (Grewal correction)" = "MMD_ANS_GRE",
#                                           "MMD - Freeman & Tukey (Freeman & Tukey correction)" = "MMD_FRE_FRE",
#                                           "MMD - Freeman & Tukey (Grewal correction)" = "MMD_FRE_GRE",
#                                           "Mahalanobis - tetrachoric correlation (TMD)" = "MAH_TMD"
#                               ),
#                               selected = "MMD_ANS"),
#                   fileInput('file1', 'Upload your data as an XLSX file',
#                             accept=c('.csv', 'application/xlsx',
#                                      'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
#                                      '.xlsx'))
#            ),
#            column(width = 3,
#                   selectInput("sex_handling", label = "Sex handling",
#                               choices = c("All individuals" = "ALL",
#                                           "Males only" = "MALE",
#                                           "Females only" = "FEMALE",
#                                           "[not ready] Sample-wise selection" = "SAMPLE",
#                                           "[not ready] Predefined selection" = "PRE"
#                               ), selected = "ALL"),
#                   selectInput("binarisation", label = "Binarization",
#                               choices = c("User defined" = "USER",
#                                           "Balanced" = "BALANCED",
#                                           "Highest chi2 statistics" = "HIGH"
#                               ),
#                               selected = "BALANCED")),
#            column(width = 3,
#                   selectInput("init_trait", label = "Initial trait selection",
#                               choices = c("All traits" = "ALL",
#                                           "Only right side" = "RIGHT",
#                                           "Only left side" = "LEFT",
#                                           "Maximum score" = "MAX",
#                                           "Minimum score" = "MIN",
#                                           "Average score" = "AVG"
#                               ),
#                               selected = "ALL"),
#                   selectInput("post_trait", label = "Post-hoc trait selection",
#                               choices = c("All traits" = "ALL",
#                                           "[not ready] Traits that differentiate" = "DIFF",
#                                           "[not ready] Traits with high inter-sample variance" = "VAR"
#                               ),
#                               selected = "ALL"))
#   ),
#   fluidRow(
#     column(width = 4,
#            plotOutput('ggMDS', width = 400, height = 400),
#            plotOutput('ggCzekanowski', width = 400, height = 400)
#     ),
#     column(width = 4,
#            plotOutput('ggClust', width = 400, height = 400),
#            plotOutput('ggPCA', width = 400, height = 400)),
#     column(width = 4,
#            downloadLink('downloadData', '* Download all matrices in a single XLSX file *'),
#            br(),
#            downloadLink('downloadFigures', '* Download all figures in a single PDF file *'),
#            #           br(),
#            #           downloadLink('downloadReport', '* Download dignostic report as an PDF file *'),
#            br(),
#            br(),
#            p("Distance matrix"),
#            verbatimTextOutput('distSummary'),
#            p("SD matrix"),
#            verbatimTextOutput('sdSummary'),
#            p("Significance matrix"),
#            verbatimTextOutput('signifSummary'))
#   ),
#   tags$head(tags$style("
#       .myRow1{background-color: #dddddd;}
#       .myRow3{height:3px; background-color: #dddddd;}
#      "
#   ))
# )

server <- function(input, output, session) {
  
  v <- reactiveValues(upload = NULL)
  
  observe({
    if (input$traitCorrelation==F){
      shinyjs::hide(id = "corFlag")
    } else if (input$traitCorrelation==T){
      shinyjs::show(id = "corFlag")
    }
  })

  
  observe({
    if (input$minNumberCheck==F){
      shinyjs::hide(id = "minNumber")
    } else if (input$minNumberCheck==T){
      shinyjs::show(id = "minNumber")
    }
  })
  
  observe({
    if (input$remTraitsCheck==F){
      shinyjs::hide(id = "remTraits")
    } else if (input$remTraitsCheck==T){
      shinyjs::show(id = "remTraits")
    }
  })
  
  observe({
    if (input$dicData==F){
      shinyjs::hide(id = "dicDataInput")
    } else if (input$dicData==T){
      shinyjs::show(id = "dicDataInput")
    }
  })
  
  observeEvent(input$descriptivesFile,{
    v$upload <- "yes"
   # print("yes")
  })
  
  inputData <- reactive({
    #Allows the user to upload a control sample
    
    if (is.null(v$upload)){
      return(NULL)
    } else {
      chosenFile <- input$descriptivesFile
      req(chosenFile)
      if (endsWith(chosenFile$name, ".xlsx")){
        descUpload <- read_excel(chosenFile$datapath, sheet = 1)
      } else if (endsWith(chosenFile$name, "csv")){
        descUpload <- read.csv(chosenFile$datapath)
      } else if (endsWith(chosenFile$name, "rds")){
        descUpload <- readRDS(chosenFile$datapath)
      }
      
      return(list(descUpload = descUpload))
    }
    
  })
  
  
  
  
  
  
  
  
  
  # inputData <- ob({
  #   #Allows the user to upload a descriptives file
  #   
  #   print(v$upload)
  #   
  #   if (is.null(v$upload)){
  #     print("yes")
  #     return(NULL)
  #    
  #   } else {
  #     chosenFile <- input$descriptivesFile
  #     print("yes")
  #     req(chosenFile)
  #     if (endsWith(chosenFile$name, ".xlsx")){
  #        descUpload <- read_excel(chosenFile$datapath, sheet = 1)
  #     } else if (endsWith(chosenFile$name, ".csv")){
  #       descUpload <- read.csv(chosenFile$datapath)
  #     } else if (endsWith(chosenFile$name, ".rds")){
  #       descUpload <- readRDS(chosenFile$datapath)
  #     }
  #     
  #     print(descUpload)
  #     
  #     return(list(descUpload = descUpload))
  #   }
  #   
  # })
  
  
  doCorrelation <- eventReactive(input$traitCorrelation, {
    
    myData <- inputData()$descUpload
    
    myData <- myData[-1,]
    
    for (i in 4:ncol(myData)){
      myData[,i] <- as.numeric(unlist(myData[,i]))
    }
    
    corMatrix <- cor(myData[,4:44],  use = "pairwise.complete.obs", method=input$corMethod)
    
    return(list(corMatrix = corMatrix))
    
  })
  
  
  
  doDescriptives <- eventReactive(input$runDescriptives, {
    
    
    if (input$groupTrait=="nogroup"){
      
      myData <- inputData()$descUpload
      
      thresholdValues <- myData[1,]
      
      myData <- myData[-1,]
      
      withProgress(message = 'Rendering table', value = 0, {
        # Number of times we'll go through the loop
        loopTimes <- ncol(myData)-4
        
        for (i in 4:ncol(myData)){
          
          
          
          myData1 <- myData %>%
            filter(!is.na(.[[i]])) %>%
            count(.[[i]]) 
          
          
          colnames(myData1) <- c(colnames(myData)[i], "n")
          
          myData1[1] <- as.numeric(unlist(myData1[1]))
          
          
          uniquescores <- myData1[1] %>%
            unique() %>%
            unlist() %>%
            sort()
          
          newData <- as.data.frame(matrix(ncol = 3, nrow = length(uniquescores)))
          colnames(newData) <- c("Trait", "Score", "n")
          
          
          for (j in 1:length(uniquescores)){
            newData$Trait[j] <- as.character(colnames(myData)[i])
            newData$Score[j] <- uniquescores[j]
          }
          
          for (k in 1:length(uniquescores)){
            cname <- colnames(newData)[3]
            ourScore <- uniquescores[k]
            ourN <- myData1 %>%
              filter_at(1, all_vars(.==ourScore)) %>%
              pull(2)
            if (identical(ourN, integer(0))){
              newData[k,3] <- 0
            } else {
              newData[k,3] <- ourN
            }
          }
          if (i==4){
            finalData <- newData
          } else {
            finalData <- rbind(finalData, newData)
          }
          
          incProgress(1/loopTimes)
          
        }
        
      })
      
      return(list(finalData = finalData))
        
      
      
      
    }
    
    if (input$groupTrait=="group 1"){
      
      myData <- inputData()$descUpload
      
      thresholdValues <- myData[1,]
      
      myData <- myData[-1,]
      
      withProgress(message = 'Rendering table', value = 0, {
        # Number of times we'll go through the loop
        loopTimes <- ncol(myData)-4
        
        for (i in 4:ncol(myData)){
          myData1 <- myData %>%
            filter(!is.na(.[[i]])) %>%
            count(GROUP1, .[[i]]) 
          
          colnames(myData1) <- c("GROUP1", colnames(myData)[i], "n")
          
          myData1[2] <- as.numeric(unlist(myData1[2]))
          
          uniquelist <- myData$GROUP1 %>%
            unique()
          
          uniquescores <- myData1[2] %>%
            unique() %>%
            unlist() %>%
            sort()
          
          newData <- as.data.frame(matrix(ncol = length(uniquelist)+2, nrow = length(uniquescores)))
          colnames(newData) <- c("Trait", "Score", uniquelist)
          
          
          for (j in 1:length(uniquescores)){
            newData$Trait[j] <- as.character(colnames(myData)[i])
            newData$Score[j] <- uniquescores[j]
          }
          
          for (j in 3:(length(uniquelist)+2)){
            for (k in 1:length(uniquescores)){
              cname <- colnames(newData)[j]
              ourScore <- uniquescores[k]
              ourN <- myData1 %>%
                filter(GROUP1==cname) %>%
                filter_at(2, all_vars(.==ourScore)) %>%
                pull(3)
              if (identical(ourN, integer(0))){
                newData[k,j] <- 0
              } else {
                newData[k,j] <- ourN
              }
            }
          }
          if (i==4){
            finalData <- newData
          } else {
            finalData <- rbind(finalData, newData)
          }
          
          incProgress(1/loopTimes)
          
        }
      })
      
      return(list(finalData = finalData))
      
    } else if (input$groupTrait=="group 2"){
      
      myData <- inputData()$descUpload
      
      thresholdValues <- myData[1,]
      
      myData <- myData[-1,]
      
      withProgress(message = 'Rendering table', value = 0, {
        # Number of times we'll go through the loop
        loopTimes <- ncol(myData)-4
      
    for (i in 4:ncol(myData)){
      
      
      
      myData1 <- myData %>%
        filter(!is.na(.[[i]])) %>%
        count(.[[3]], .[[i]]) 
      
      
      colnames(myData1) <- c("GROUP2", colnames(myData)[i], "n")
      
      myData1[2] <- as.numeric(unlist(myData1[2]))
      
      uniquelist <- myData$GROUP2 %>%
        unique()
      
      uniquescores <- myData1[2] %>%
        unique() %>%
        unlist() %>%
        sort()
      
      newData <- as.data.frame(matrix(ncol = length(uniquelist)+2, nrow = length(uniquescores)))
      colnames(newData) <- c("Trait", "Score", uniquelist)
      
      
      for (j in 1:length(uniquescores)){
        newData$Trait[j] <- as.character(colnames(myData)[i])
        newData$Score[j] <- uniquescores[j]
      }
      
      for (j in 3:(length(uniquelist)+2)){
        for (k in 1:length(uniquescores)){
          cname <- colnames(newData)[j]
          ourScore <- uniquescores[k]
          ourN <- myData1 %>%
            filter(GROUP2==cname) %>%
            filter_at(2, all_vars(.==ourScore)) %>%
            pull(3)
          if (identical(ourN, integer(0))){
            newData[k,j] <- 0
          } else {
            newData[k,j] <- ourN
          }
        }
      }
      if (i==4){
        finalData <- newData
      } else {
        finalData <- rbind(finalData, newData)
      }
      
      incProgress(1/loopTimes)
    }
    
    return(list(finalData = finalData))
    
    

  })
    } else if (input$groupTrait=="bothgroups"){
      
      myData <- inputData()$descUpload
      
      thresholdValues <- myData[1,]
      
      myData <- myData[-1,]
      
      
      combinedGroup <- rep(NA, nrow(myData))
      
      for (i in 1:nrow(myData)){
        combinedGroup[i] <- paste0(myData[i,]$GROUP1, "+", myData[i,]$GROUP2)
      }
      
      myData$combinedGroup <- combinedGroup
      
      myData <- myData[,c(1:3, ncol(myData), 4:(ncol(myData)-1))]
      
      withProgress(message = 'Rendering table', value = 0, {
        # Number of times we'll go through the loop
        loopTimes <- ncol(myData)-5
      
      for (i in 5:ncol(myData)){
        
        myData1 <- myData %>%
          filter(!is.na(.[[i]])) %>%
          count(.[[4]],   .[[i]]) 
        
        
        colnames(myData1) <- c("GROUP1+GROUP2", colnames(myData)[i], "n")
        
        myData1[2] <- as.numeric(unlist(myData1[2]))
        
        uniquelist <- myData$combinedGroup %>%
          unique()
        
        uniquescores <- myData1[2] %>%
          unique() %>%
          unlist() %>%
          sort()
        
        newData <- as.data.frame(matrix(ncol = length(uniquelist)+2, nrow = length(uniquescores)))
        colnames(newData) <- c("Trait", "Score", uniquelist)
        
        
        for (j in 1:length(uniquescores)){
          newData$Trait[j] <- as.character(colnames(myData)[i])
          newData$Score[j] <- uniquescores[j]
        }
        
        for (j in 3:(length(uniquelist)+2)){
          for (k in 1:length(uniquescores)){
            cname <- colnames(newData)[j]
            ourScore <- uniquescores[k]
            ourN <- myData1 %>%
              filter(`GROUP1+GROUP2`==cname) %>%
              filter_at(2, all_vars(.==ourScore)) %>%
              pull(3)
            if (identical(ourN, integer(0))){
              newData[k,j] <- 0
            } else {
              newData[k,j] <- ourN
            }
          }
        }
        if (i==5){
          finalData <- newData
        } else {
          finalData <- rbind(finalData, newData)
        }
        
        incProgress(1/loopTimes)
        
      }
      
      return(list(finalData = finalData))
      })
      
      
    }
    
    
  })
  
  
  output$descTable <- renderTable({
    
    shinyjs::show(id = "downloadDescExcel")
    shinyjs::show(id = "downloadDescCSV")
    
    doDescriptives()$finalData
    
  })
  
  output$corTable <- renderDataTable({
    
    rowCallback <- c(
      "function(row, data){",
      "  for(var i=0; i<data.length; i++){",
      "    if(data[i] === null){",
      "      $('td:eq('+i+')', row).html('NA')",
      "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
      "    }",
      "  }",
      "}"  
    )
    
   data <-  doCorrelation()$corMatrix
    
    datatable(data, options = list(rowCallback = JS(rowCallback))) %>% formatStyle(
      columns = colnames(data),
      backgroundColor = styleInterval(c(input$corFlag, 0.99), c('white', 'lightgreen', 'lightblue'))
    ) %>%
      formatSignif(
        columns = colnames(data),
        digits = 3
      )
    
    
    
    
    
    # data %>%
    #   tableHTML(rownames = T) %>%
    # add_css_conditional_column(conditional = 'between',
    #                            between = c(0.5, 1),
    #                            css = list(c('background-color'),
    #                                       c('blue')),
    #                            columns = 1:ncol(data))
    
  })
  
  
  
  
  output$downloadDescExcel <- downloadHandler(
    filename = function() {
      "modifiedDesc.xlsx"
    },
    
    content = function(file) {
      
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Sheet 1"
      )
      
      writeData(
        
        my_workbook,
        sheet = 1,
        doDescriptives()$finalData
      )
      
      saveWorkbook(my_workbook, file)
      
      
    }
    
  
  )
  
  output$downloadDescCSV <- downloadHandler(
    filename = function() {
      "modifiedDesc.csv"
    },
    content = function(file) {
      write.csv(doDescriptives()$finalData, file, row.names = FALSE)
    }
  )
  
  
  
  
  rawdataInput <- reactive({
    inFile <- input$file1
    if (input$button & is.null(inFile)) {
      df <- v$exampleFile
    } else {
      if (is.null(inFile))
        return(NULL)
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
    }
    
    # side handling
    if (input$init_trait == "RIGHT") {
      df <- df[,c(1:3,seq(4, ncol(df), 2))]
    }
    if (input$init_trait == "LEFT") {
      df <- df[,c(1:3,seq(5, ncol(df), 2))]
    }
    if (input$init_trait == "MIN") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmin(df[,i], df[,i-1], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_trait == "MAX") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmax(df[,i], df[,i-1], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_trait == "AVG") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- rowMeans(df[,i+c(-1,0)], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    
    # binarisation
    THRESHOLD = df[1,]
    if (input$binarisation == "USER") {
      df <- df[-1,]
    }
    
    # sex handling
    if (input$sex_handling == "MALE") {
      df <- df[which(df[,3] == "M"),]
    }
    if (input$sex_handling == "FEMALE") {
      df <- df[which(df[,3] == "F"),]
    }
    
    # binarisation
    if (input$binarisation == "BALANCED") {
      THRESHOLD = df[1,]
      for (i in 4:ncol(df)) {
        THRESHOLD[1,i] <- median(df[,i], na.rm=TRUE)
      }
    }
    
    if (input$binarisation == "HIGH") {
      THRESHOLD = df[1,]
      for (i in 4:ncol(df)) {
        cutoffs <- sort(unique(na.omit(df[,i])))
        if (length(cutoffs) > 1) {
          p.vals <- sapply(2:length(cutoffs), function(j) {
            chisq.test(table(df[,i] >= cutoffs[j], df[,2]))$statistic[[1]]
          })
          if (any(is.nan(p.vals)))
            p.vals[is.nan(p.vals)] = 1
          THRESHOLD[1,i] <- cutoffs[which.max(p.vals)+1]
        } else {
          THRESHOLD[1,i] <- median(df[,i], na.rm=TRUE)
        }
      }
      
    }
    
    list(df = df, THRESHOLD = THRESHOLD)
  })
  # get data
  dataInput <- reactive({
    ll <- rawdataInput()
    if (is.null(ll)) return(NULL)
    df <- ll$df
    THRESHOLD <- ll$THRESHOLD
    # remove without site
    df <- df[!is.na(df[,2]),]
    
    # binarisation
    if (input$binarisation %in% c("USER", "BALANCED", "HIGH")) {
      for (i in 4:ncol(df)) {
        df[,i] <- (df[,i] >= THRESHOLD[1,i]) + 0
      }
    }
    df
  })
  
  # get dist
  getDist <- reactive({
    res <- NULL
    
    df <- dataInput()
    
    selected <- strsplit(input$method_sel, split="_")[[1]]
    if (selected[1] == "MMD") {
      theta <- dentalAffinities::theta_Anscombe
      if (selected[2] == "FRE")
        theta <- dentalAffinities::theta_Freeman
      
      thetadiff <- dentalAffinities::thetadiff_uncorrected
      if (selected[3] == "FRE")
        thetadiff <- dentalAffinities::thetadiff_Freeman
      if (selected[3] == "GRE")
        thetadiff <- dentalAffinities::thetadiff_Grewal
      
      tmp <- dentalAffinities::get_Mn_Mp(df)
      res <- dentalAffinities::calculateMMD(data.frame(tmp$Mn), as.data.frame(tmp$Mp), thetadiff, theta)
    }
    if (selected[1] == "MAH") {
      res <- dentalAffinities::calculateD2(df)
    }
    res
  })
  
  # table
  output$ggMDS <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDist()
    dentalAffinities::getMDS(mat$MMDMatrix)
  })
  output$ggCzekanowski <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDist()
    dentalAffinities::getCzekanowski(mat$MMDMatrix)
  })
  output$ggPCA <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    dentalAffinities::getPCA(di)
  })
  
  # table
  output$ggClust <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDist()
    dentalAffinities::getClust(mat$MMDMatrix)
  })
  
  output$distSummary <- renderPrint({
    di <- dataInput()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDist()$MMDMatrix
      print(round(mat, 2))
    }
  })
  output$sdSummary <- renderPrint({
    di <- dataInput()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDist()$SDMatrix
      if (is.null(mat)) {
        print("SD matrix not is available")
      } else {
        print(round(mat, 2))
      }
    }
  })
  output$signifSummary <- renderPrint({
    di <- dataInput()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDist()$SigMatrix
      if (is.null(mat)) {
        print("P-values matrix not is available")
      } else {
        print(round(mat, 5))
      }
    }
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('dentalAffinities_diagnostic_', Sys.Date(), '.pdf', sep='')
    },
    content = function(con) {
      ll <- rawdataInput()
      if (!is.null(ll)) {
        df <- ll$df
        THRESHOLD <- ll$THRESHOLD
        
        src <- normalizePath('report.Rmd')
        td <- tempdir()
        owd <- setwd(td)
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        save(df, THRESHOLD, file="raw_data.rda")
        
        library(rmarkdown)
        out <- render('report.Rmd', pdf_document())
        file.rename(out, con)
      }
    }
  )
  
  output$downloadFigures <- downloadHandler(
    filename = function() {
      paste('dentalAffinities_', Sys.Date(), '.pdf', sep='')
    },
    content = function(con) {
      di <- dataInput()
      if (!is.null(di)) {
        pdf(file = con, width = 10, height = 10)
        mat <- getDist()
        
        print(dentalAffinities::getClust(mat$MMDMatrix))
        print(dentalAffinities::getMDS(mat$MMDMatrix))
        print(dentalAffinities::getCzekanowski(mat$MMDMatrix))
        print(dentalAffinities::getPCA(di))
        
        dev.off()
      }
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('dentalAffinities_', Sys.Date(), '.xlsx', sep='')
    },
    content = function(con) {
      di <- dataInput()
      if (is.null(di)) {
        mat <- data.frame("Upload the data first")
      } else {
        rdi <- rawdataInput()
        
        # basic stats
        parameter = c("File name",
                      "Method",
                      "Sex handling",
                      "Binarization",
                      "Initial trait selection",
                      "Post-hoc trait selection",
                      "Date")
        value = c(ifelse(is.null(input$file1), "Example data", input$file1[[1]]),
                  names(which(input$method_sel == method_sel)),
                  names(which(input$sex_handling == sex_handling)),
                  names(which(input$binarisation == binarisation)),
                  names(which(input$init_trait == init_trait)),
                  names(which(input$post_trait == post_trait)),
                  date())
        
        df <- list(parameter=parameter,
                   value=value)
        mat <- list(Basic_Statics = df, Thresholds = t(rdi$THRESHOLD[1,-(1:3)]))
        tmp <- dentalAffinities::get_Mn_Mp(di)
        tmpN <- t(tmp[[1]])
        colnames(tmpN) <- paste(tmpN[1,], " (N)")
        tmpP <- t(data.frame(tmp[[2]][,1], round(tmp[[2]][,-1], 3)))
        colnames(tmpP) <- paste(tmpP[1,], " (prc)")
        tmp2 <- cbind(tmpN[-1,], tmpP[-1,])
        tmp3 <- apply(tmp2, 2, as.numeric)
        rownames(tmp3) <- rownames(tmp2)
        mat$Counts_in_sites <- tmp3
        stats <- getCutoffStats(rdi$df)
        mat$Cutoff_statistics <- stats
        freq <- getSummaryStatistics(rdi$df)
        mat$Frequencies <- freq
        mat2 <- getDist()
        mat <- c(mat, mat2)
        
        write.xlsx(mat, file = con, row.names = TRUE)
      }
    }
  )
  
}

shinyApp(ui, server)
