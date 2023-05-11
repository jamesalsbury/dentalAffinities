library(shiny)
library(dentalAffinities)
library(MASS)
library(openxlsx)
library(ggbiplot)
library(shinyjs)
library(tidyverse)
library(readxl)
library(tableHTML)
library(DT)
library(vegan)
library(pairwiseAdonis)

#source("functions.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Dental Affinities"),
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Descriptives",
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
      tabPanel("Analysis - MMD",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("descriptivesFile1", label = "Please upload a data file here"),
                   selectInput("method_selMMD", label = "Method selection",
                               choices = c("MMD - Anscombe" = "MMD_ANS_0",
                                           "MMD - Freeman & Tukey" = "MMD_FRE_0",
                                           "MMD - Anscombe (Freeman & Tukey correction)" = "MMD_ANS_FRE",
                                           "MMD - Anscombe (Grewal correction)" = "MMD_ANS_GRE",
                                           "MMD - Freeman & Tukey (Freeman & Tukey correction)" = "MMD_FRE_FRE",
                                           "MMD - Freeman & Tukey (Grewal correction)" = "MMD_FRE_GRE"
                               ),
                               selected = "MMD_ANS"),
                   selectInput("init_traitMMD", label = "Initial trait selection",
                               choices = c("All traits" = "ALL",
                                           "Only right side" = "RIGHT",
                                           "Only left side" = "LEFT",
                                           "Maximum score" = "MAX",
                                           "Minimum score" = "MIN",
                                           "Average score" = "AVG"
                               ),
                               selected = "ALL"),
                   selectInput("binarisationMMD", label = "Binarization",
                               choices = c("User defined" = "USER",
                                           "Balanced" = "BALANCED",
                                           "Highest chi2 statistics" = "HIGH"
                               ),
                               selected = "BALANCED"),
                   selectInput("groupChoiceMMD", label = "Plots show",
                               choices = c("Group 1" = "group1",
                                           "Group 2" = "group2",
                                           "Group 1 + Group 2" = "bothgroups"),
                               selected = "group1"),
                   checkboxGroupInput("group_handling1MMD", label = "Group 1 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL"),
                   checkboxGroupInput("group_handling2MMD", label = "Group 2 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL"),
                   checkboxInput("minNumberCheckMMD", label = "Minimum number of observations/groups"),
                   hidden(numericInput("minNumberMMD", label = "Min", value = 10)),
                   checkboxInput("remTraitsCheckMMD", label = "Remove traits exhibiting no variation"),
                   hidden(numericInput("remTraitsMMD", label = "MD<", value = 0)),
                   actionButton("runAnalysisMMD", "Run")
                 ),
                 mainPanel = mainPanel(
                   fluidRow(
                     column(width = 4,
                            plotOutput('ggMDSMMD', width = 200, height = 200),
                            plotOutput('ggCzekanowskiMMD', width = 200, height = 200)
                     ),
                     column(width = 4,
                            plotOutput('ggClustMMD', width = 200, height = 200),
                            plotOutput('ggPCAMMD', width = 200, height = 200)),
                     column(width = 4,
                            br(),
                            br(),
                            p("Distance matrix"),
                            verbatimTextOutput('distSummaryMMD'),
                            p("SD matrix"),
                            verbatimTextOutput('sdSummaryMMD'),
                            p("Significance matrix"),
                            verbatimTextOutput('signifSummaryMMD'))
                   ),
                 )
               )
               
      ),
      tabPanel("Analysis - Mahalanobis",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("descriptivesFile2", label = "Please upload a data file here"),
                   selectInput("method_selMahalanobis", label = "Method selection",
                               choices = c("Mahalanobis - tetrachoric correlation (TMD)" = "MAH_TMD"
                               ),
                               selected = "MAH_TMF"),
                   selectInput("init_traitMahalanobis", label = "Initial trait selection",
                               choices = c("All traits" = "ALL",
                                           "Only right side" = "RIGHT",
                                           "Only left side" = "LEFT",
                                           "Maximum score" = "MAX",
                                           "Minimum score" = "MIN",
                                           "Average score" = "AVG"
                               ),
                               selected = "ALL"),
                   selectInput("binarisationMahalanobis", label = "Binarization",
                               choices = c("User defined" = "USER",
                                           "Balanced" = "BALANCED",
                                           "Highest chi2 statistics" = "HIGH"
                               ),
                               selected = "BALANCED"),
                   checkboxGroupInput("group_handling1Mahalanobis", label = "Group 1 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL"),
                   checkboxGroupInput("group_handling2Mahalanobis", label = "Group 2 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL"),
                   checkboxInput("minNumberCheckMahalanobis", label = "Minimum number of observations/groups"),
                   hidden(numericInput("minNumberMahalanobis", label = "Min", value = 10)),
                   checkboxInput("remTraitsCheckMahalanobis", label = "Remove traits exhibiting no variation"),
                   hidden(numericInput("remTraitsMahalanobis", label = "MD<", value = 0)),
                   actionButton("runAnalysisMahalanobis", "Run")
                 ),
                 mainPanel = mainPanel(
                   fluidRow(
                     column(width = 4,
                            plotOutput('ggMDSMahalanobis', width = 200, height = 200),
                            plotOutput('ggCzekanowskiMahalanobis', width = 200, height = 200)
                     ),
                     column(width = 4,
                            plotOutput('ggClustMahalanobis', width = 200, height = 200),
                            plotOutput('ggPCAMahalanobis', width = 200, height = 200)),
                     column(width = 4,
                            br(),
                            br(),
                            p("Distance matrix"),
                            verbatimTextOutput('distSummaryMahalanobis'),
                            p("SD matrix"),
                            verbatimTextOutput('sdSummaryMahalanobis'),
                            p("Significance matrix"),
                            verbatimTextOutput('signifSummaryMahalanobis'))
                   ),
                 )
               )
               
      ),
      tabPanel("Analysis - Gower",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("descriptivesFile3", label = "Please upload a data file here"),
                   selectInput("method_selGower", label = "Method selection",
                               choices = c("Gower" = "gower"
                               ),
                               selected = "gower"),
                   selectInput("init_traitGower", label = "Initial trait selection",
                               choices = c("All traits" = "ALL",
                                           "Only right side" = "RIGHT",
                                           "Only left side" = "LEFT",
                                           "Maximum score" = "MAX",
                                           "Minimum score" = "MIN",
                                           "Average score" = "AVG"
                               ),
                               selected = "ALL"),
                   selectInput("binarisationGower", label = "Binarization",
                               choices = c("User defined" = "USER",
                                           "Balanced" = "BALANCED",
                                           "Highest chi2 statistics" = "HIGH"
                               ),
                               selected = "BALANCED"),
                   checkboxGroupInput("group_handling1Gower", label = "Group 1 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL"),
                   checkboxGroupInput("group_handling2Gower", label = "Group 2 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL"),
                   checkboxInput("minNumberCheckGower", label = "Minimum number of observations/groups"),
                   hidden(numericInput("minNumberGower", label = "Min", value = 10)),
                   checkboxInput("remTraitsCheckGower", label = "Remove traits exhibiting no variation"),
                   hidden(numericInput("remTraitsGower", label = "MD<", value = 0)),
                   actionButton("runAnalysisGower", "Run")
                 ),
                 mainPanel = mainPanel(
                   fluidRow(
                     column(width = 4,
                            plotOutput('ggMDSGower', width = 200, height = 200),
                            plotOutput('ggCzekanowskiGower', width = 200, height = 200)
                     ),
                     column(width = 4,
                            plotOutput('ggClustGower', width = 200, height = 200),
                            plotOutput('ggPCAGower', width = 200, height = 200)),
                     column(width = 4,
                            br(),
                            br(),
                            p("Distance matrix"),
                            verbatimTextOutput('distSummaryGower'),
                            p("SD matrix"),
                            verbatimTextOutput('sdSummaryGower'),
                            p("Significance matrix"),
                            verbatimTextOutput('signifSummaryGower'))
                   ),
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
    if (input$minNumberCheckMMD==F){
      shinyjs::hide(id = "minNumber")
    } else if (input$minNumberCheckMMD==T){
      shinyjs::show(id = "minNumber")
    }
  })
  
  observe({
    if (input$remTraitsCheckMMD==F){
      shinyjs::hide(id = "remTraits")
    } else if (input$remTraitsCheckMMD==T){
      shinyjs::show(id = "remTraits")
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
    
    corMatrix <- cor(myData[,4:ncol(myData)],  use = "pairwise.complete.obs", method=input$corMethod)
    
    for (i in 4:(ncol(myData)-1)){
      for (k in (i+1):ncol(myData)){
        corMatrix[k-3, i-3] <- nrow(na.omit(myData[,c(i, k)]))
      }
    }
    
    
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
      backgroundColor = styleInterval(c(input$corFlag, 0.999, 1.001, 1000), c('white', 'lightgreen', 'lightblue', 'lightyellow', 'red'))
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
  
  
  observe({
    
    inFile <- input$descriptivesFile1
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
      df <- df[-1,]
      uniquenamesgroup1 <- unique(df[,2])
      
      
    }
    
    updateCheckboxGroupInput(session, inputId = "group_handling1MMD", choices = uniquenamesgroup1, selected = uniquenamesgroup1)
    
    
  })
  
  observeEvent(input$group_handling1MMD,{
    
    inFile <- input$descriptivesFile1
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
      df <- df[-1,]
      uniquenamesgroup1 <- unique(df[,2])
      
    }
    
    # group handling
    group1list <- input$group_handling1MMD
    
    #print(group1list)
    
    for (i in 1:length(group1list)){
      if (i==1){
        temp <- df[which(df[,2] == group1list[i]),]
      } else {
        temp <- rbind(temp, df[which(df[,2] == group1list[i]),])
      }
      
    }
    
    df <- temp
    
    uniquenamesgroup2 <- unique(df[,3])
    
    updateCheckboxGroupInput(session, inputId = "group_handling2MMD", choices = uniquenamesgroup2, selected = uniquenamesgroup2)
    
  })
  
  
  
  rawdataInputMMD <- reactive({
    inFile <- input$descriptivesFile1
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
    }
    
    
    # side handling
    if (input$init_traitMMD == "RIGHT") {
      df <- df[,c(1:3,seq(4, ncol(df), 2))]
    }
    if (input$init_traitMMD == "LEFT") {
      df <- df[,c(1:3,seq(5, ncol(df), 2))]
    }
    if (input$init_traitMMD== "MIN") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmin(df[,i], df[,i-1], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_traitMMD == "MAX") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmax(df[,i], df[,i-1], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_traitMMD == "AVG") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- rowMeans(df[,i+c(-1,0)], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    
    
    
    # binarisation
    THRESHOLD = df[1,]
    if (input$binarisationMMD == "USER") {
      df <- df[-1,]
    }
    
    # binarisation
    if (input$binarisationMMD == "BALANCED") {
      THRESHOLD = df[1,]
      for (i in 4:ncol(df)) {
        THRESHOLD[1,i] <- median(df[,i], na.rm=TRUE)
      }
    }
    
    if (input$binarisationMMD == "HIGH") {
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
    
    group1list <- input$group_handling1MMD
    
    #print(group1list)
    
    for (i in 1:length(group1list)){
      if (i==1){
        temp <- df[which(df[,2] == group1list[i]),]
      } else {
        temp <- rbind(temp, df[which(df[,2] == group1list[i]),])
      }
      
    }
    
    df <- temp
    
    group2list <- input$group_handling2MMD
    
    for (i in 1:length(group2list)){
      if (i==1){
        temp <- df[which(df[,3] == group2list[i]),]
      } else {
        temp <- rbind(temp, df[which(df[,3] == group2list[i]),])
      }
      
    }
    
    df <- temp
    
    
    
    list(df = df, THRESHOLD = THRESHOLD)
  })
  # get data
  dataInputMMD <- reactive({
    ll <- rawdataInputMMD()
    if (is.null(ll)) return(NULL)
    df <- ll$df
    THRESHOLD <- ll$THRESHOLD
    # remove without site
    df <- df[!is.na(df[,2]),]
    
    #binarisation
    if (input$binarisationMMD %in% c("USER", "BALANCED", "HIGH")) {
      for (i in 4:ncol(df)) {
        df[,i] <- (df[,i] >= THRESHOLD[1,i]) + 0
      }
    }
    df
  })
  
  # get dist
  getDistMMD <- reactive({
    res <- NULL
    
    df <- dataInputMMD()
    
    selected <- strsplit(input$method_selMMD, split="_")[[1]]
    if (selected[1] == "MMD") {
      theta <- dentalAffinities::theta_Anscombe
      if (selected[2] == "FRE")
        theta <- dentalAffinities::theta_Freeman
      
      thetadiff <- dentalAffinities::thetadiff_uncorrected
      if (selected[3] == "FRE")
        thetadiff <- dentalAffinities::thetadiff_Freeman
      if (selected[3] == "GRE")
        thetadiff <- dentalAffinities::thetadiff_Grewal
      
      if (input$groupChoiceMMD=="group1"){tmp <- get_Mn_Mp_group_1(df)}
      if (input$groupChoiceMMD=="group2"){tmp <- get_Mn_Mp_group_2(df)}
      if (input$groupChoiceMMD=="bothgroups"){tmp <- get_Mn_Mp_bothgroups(df)}
      
      
      
      res <- dentalAffinities::calculateMMD(data.frame(tmp$Mn), as.data.frame(tmp$Mp), thetadiff, theta)
    }
    if (selected[1] == "MAH") {
      res <- dentalAffinities::calculateD2(df)
    }
    print(res)
    res
  })
  
  # table
  output$ggMDSMMD <- renderPlot({
    di <- dataInputMMD()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMMD()
    dentalAffinities::getMDS(mat$MMDMatrix)
  })
  output$ggCzekanowskiMMD <- renderPlot({
    di <- dataInputMMD()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMMD()
    dentalAffinities::getCzekanowski(mat$MMDMatrix)
  })
  output$ggPCAMMD <- renderPlot({
    di <- dataInputMMD()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    dentalAffinities::getPCA(di)
  })
  
  # table
  output$ggClustMMD <- renderPlot({
    di <- dataInputMMD()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMMD()
    dentalAffinities::getClust(mat$MMDMatrix)
  })
  
  output$distSummaryMMD <- renderPrint({
    di <- dataInputMMD()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMMD()$MMDMatrix
      print(round(mat, 2))
    }
  })
  output$sdSummaryMMD <- renderPrint({
    di <- dataInputMMD()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMMD()$SDMatrix
      if (is.null(mat)) {
        print("SD matrix not is available")
      } else {
        print(round(mat, 2))
      }
    }
  })
  output$signifSummaryMMD <- renderPrint({
    di <- dataInputMMD()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMMD()$SigMatrix
      if (is.null(mat)) {
        print("P-values matrix not is available")
      } else {
        print(round(mat, 5))
      }
    }
  })
  
  #Mahalanobis functions
  
  observe({
    
    inFile <- input$descriptivesFile2
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
      df <- df[-1,]
      uniquenamesgroup1 <- unique(df[,2])
      
      
    }
    
    updateCheckboxGroupInput(session, inputId = "group_handling1Mahalanobis", choices = uniquenamesgroup1, selected = uniquenamesgroup1)
    
    
  })
  
  observeEvent(input$group_handling1Mahalanobis,{
    
    inFile <- input$descriptivesFile2
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
      df <- df[-1,]
      uniquenamesgroup1 <- unique(df[,2])
      
    }
    
    # group handling
    group1list <- input$group_handling1Mahalanobis
    
    #print(group1list)
    
    for (i in 1:length(group1list)){
      if (i==1){
        temp <- df[which(df[,2] == group1list[i]),]
      } else {
        temp <- rbind(temp, df[which(df[,2] == group1list[i]),])
      }
      
    }
    
    df <- temp
    
    uniquenamesgroup2 <- unique(df[,3])
    
    updateCheckboxGroupInput(session, inputId = "group_handling2Mahalanobis", choices = uniquenamesgroup2, selected = uniquenamesgroup2)
    
  })
  
  
  
  rawdataInputMahalanobis <- reactive({
    inFile <- input$descriptivesFile2
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
    }
    
    
    # side handling
    if (input$init_traitMahalanobis == "RIGHT") {
      df <- df[,c(1:3,seq(4, ncol(df), 2))]
    }
    if (input$init_traitMahalanobis == "LEFT") {
      df <- df[,c(1:3,seq(5, ncol(df), 2))]
    }
    if (input$init_traitMahalanobis== "MIN") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmin(df[,i], df[,i-1], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_traitMahalanobis == "MAX") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmax(df[,i], df[,i-1], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_traitMahalanobis == "AVG") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- rowMeans(df[,i+c(-1,0)], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    
    
    
    # binarisation
    THRESHOLD = df[1,]
    if (input$binarisationMahalanobis == "USER") {
      df <- df[-1,]
    }
    
    # binarisation
    if (input$binarisationMahalanobis == "BALANCED") {
      THRESHOLD = df[1,]
      for (i in 4:ncol(df)) {
        THRESHOLD[1,i] <- median(df[,i], na.rm=TRUE)
      }
    }
    
    if (input$binarisationMahalanobis == "HIGH") {
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
    
    group1list <- input$group_handling1Mahalanobis
    
    #print(group1list)
    
    for (i in 1:length(group1list)){
      if (i==1){
        temp <- df[which(df[,2] == group1list[i]),]
      } else {
        temp <- rbind(temp, df[which(df[,2] == group1list[i]),])
      }
      
    }
    
    df <- temp
    
    group2list <- input$group_handling2Mahalanobis
    
    for (i in 1:length(group2list)){
      if (i==1){
        temp <- df[which(df[,3] == group2list[i]),]
      } else {
        temp <- rbind(temp, df[which(df[,3] == group2list[i]),])
      }
      
    }
    
    df <- temp
    
    
    
    list(df = df, THRESHOLD = THRESHOLD)
  })
  # get data
  dataInputMahalanobis <- reactive({
    ll <- rawdataInputMahalanobis()
    if (is.null(ll)) return(NULL)
    df <- ll$df
    THRESHOLD <- ll$THRESHOLD
    # remove without site
    df <- df[!is.na(df[,2]),]
    
    #binarisation
    if (input$binarisationMahalanobis %in% c("USER", "BALANCED", "HIGH")) {
      for (i in 4:ncol(df)) {
        df[,i] <- (df[,i] >= THRESHOLD[1,i]) + 0
      }
    }
    df
  })
  
  # get dist
  getDistMahalanobis <- reactive({
    res <- NULL
    
    df <- dataInputMahalanobis()
    
    selected <- strsplit(input$method_selMahalanobis, split="_")[[1]]
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
  output$ggMDSMahalanobis <- renderPlot({
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMahalanobis()
    dentalAffinities::getMDS(mat$MMDMatrix)
  })
  output$ggCzekanowskiMahalanobis <- renderPlot({
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMahalanobis()
    dentalAffinities::getCzekanowski(mat$MMDMatrix)
  })
  output$ggPCAMahalanobis <- renderPlot({
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    dentalAffinities::getPCA(di)
  })
  
  # table
  output$ggClustMahalanobis <- renderPlot({
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMahalanobis()
    dentalAffinities::getClust(mat$MMDMatrix)
  })
  
  output$distSummaryMahalanobis <- renderPrint({
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMahalanobis()$MMDMatrix
      print(round(mat, 2))
    }
  })
  output$sdSummaryMahalanobis <- renderPrint({
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMahalanobis()$SDMatrix
      if (is.null(mat)) {
        print("SD matrix not is available")
      } else {
        print(round(mat, 2))
      }
    }
  })
  output$signifSummaryMahalanobis <- renderPrint({
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMahalanobis()$SigMatrix
      if (is.null(mat)) {
        print("P-values matrix not is available")
      } else {
        print(round(mat, 5))
      }
    }
  })
  
  #Gower functions

  observe({
    
    inFile <- input$descriptivesFile3
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
      df <- df[-1,]
      uniquenamesgroup1 <- unique(df[,2])
      
      
    }
    
    updateCheckboxGroupInput(session, inputId = "group_handling1Gower", choices = uniquenamesgroup1, selected = uniquenamesgroup1)
    
    
  })
  
  observeEvent(input$group_handling1Gower,{
    
    inFile <- input$descriptivesFile2
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
      df <- df[-1,]
      uniquenamesgroup1 <- unique(df[,2])
      
    }
    
    # group handling
    group1list <- input$group_handling1Gower
    
    #print(group1list)
    
    for (i in 1:length(group1list)){
      if (i==1){
        temp <- df[which(df[,2] == group1list[i]),]
      } else {
        temp <- rbind(temp, df[which(df[,2] == group1list[i]),])
      }
      
    }
    
    df <- temp
    
    uniquenamesgroup2 <- unique(df[,3])
    
    updateCheckboxGroupInput(session, inputId = "group_handling2Gower", choices = uniquenamesgroup2, selected = uniquenamesgroup2)
    
  })
  
  
  
  rawdataInputGower <- reactive({
    inFile <- input$descriptivesFile2
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
    }
    
    
    # side handling
    if (input$init_traitGower == "RIGHT") {
      df <- df[,c(1:3,seq(4, ncol(df), 2))]
    }
    if (input$init_traitGower == "LEFT") {
      df <- df[,c(1:3,seq(5, ncol(df), 2))]
    }
    if (input$init_traitGower== "MIN") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmin(df[,i], df[,i-1], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_traitGower == "MAX") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmax(df[,i], df[,i-1], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_traitGower == "AVG") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- rowMeans(df[,i+c(-1,0)], na.rm = TRUE)
        colnames(df)[2 + (i-1)/2] <- colnames(df)[i]
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    
    
    
    # binarisation
    THRESHOLD = df[1,]
    if (input$binarisationGower == "USER") {
      df <- df[-1,]
    }
    
    # binarisation
    if (input$binarisationGower == "BALANCED") {
      THRESHOLD = df[1,]
      for (i in 4:ncol(df)) {
        THRESHOLD[1,i] <- median(df[,i], na.rm=TRUE)
      }
    }
    
    if (input$binarisationGower == "HIGH") {
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
    
    group1list <- input$group_handling1Gower
    
    #print(group1list)
    
    for (i in 1:length(group1list)){
      if (i==1){
        temp <- df[which(df[,2] == group1list[i]),]
      } else {
        temp <- rbind(temp, df[which(df[,2] == group1list[i]),])
      }
      
    }
    
    df <- temp
    
    group2list <- input$group_handling2Gower
    
    for (i in 1:length(group2list)){
      if (i==1){
        temp <- df[which(df[,3] == group2list[i]),]
      } else {
        temp <- rbind(temp, df[which(df[,3] == group2list[i]),])
      }
      
    }
    
    df <- temp
    
    
    
    list(df = df, THRESHOLD = THRESHOLD)
  })
  # get data
  dataInputGower <- reactive({
    ll <- rawdataInputGower()
    if (is.null(ll)) return(NULL)
    df <- ll$df
    THRESHOLD <- ll$THRESHOLD
    # remove without site
    df <- df[!is.na(df[,2]),]
    
    #binarisation
    if (input$binarisationGower %in% c("USER", "BALANCED", "HIGH")) {
      for (i in 4:ncol(df)) {
        df[,i] <- (df[,i] >= THRESHOLD[1,i]) + 0
      }
    }
    df
  })
  
  # get dist
  getDistGower <- reactive({
    res <- NULL
    
    df <- dataInputGower()
    
    selected <- strsplit(input$method_selGower, split="_")[[1]]
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
  output$ggMDSGower <- renderPlot({
    di <- dataInputGower()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistGower()
    dentalAffinities::getMDS(mat$MMDMatrix)
  })
  output$ggCzekanowskiGower <- renderPlot({
    di <- dataInputGower()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistGower()
    dentalAffinities::getCzekanowski(mat$MMDMatrix)
  })
  output$ggPCAGower <- renderPlot({
    di <- dataInputGower()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    dentalAffinities::getPCA(di)
  })
  
  # table
  output$ggClustGower <- renderPlot({
    di <- dataInputGower()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistGower()
    dentalAffinities::getClust(mat$MMDMatrix)
  })
  
  output$distSummaryGower <- renderPrint({
    di <- dataInputGower()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistGower()$MMDMatrix
      print(round(mat, 2))
    }
  })
  output$sdSummaryGower <- renderPrint({
    di <- dataInputGower()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistGower()$SDMatrix
      if (is.null(mat)) {
        print("SD matrix not is available")
      } else {
        print(round(mat, 2))
      }
    }
  })
  output$signifSummaryGower <- renderPrint({
    di <- dataInputGower()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistGower()$SigMatrix
      if (is.null(mat)) {
        print("P-values matrix not is available")
      } else {
        print(round(mat, 5))
      }
    }
  })
  
  get_Mn_Mp_group_1 <- function(binary_trait_data) {
    colnames(binary_trait_data)[1:3] <- c("id", "group1", "group2")
    binary_trait_data_long <- gather(binary_trait_data, trait, value, -(1:3))
    gr <- summarise(group_by(binary_trait_data_long, group1, trait),
                    n = length(na.omit(value)),
                    p = ifelse(n>0, mean(value > 0, na.rm=TRUE),0.5))
    Mn <- spread(gr[,c("group1","trait", "n")], trait, n)
    Mp <- spread(gr[,c("group1","trait", "p")], trait, p)

    list(Mn = Mn, Mp = Mp)
  }
  
  get_Mn_Mp_group_2 <- function(binary_trait_data) {
    colnames(binary_trait_data)[1:3] <- c("id", "group1", "group2")
    binary_trait_data_long <- gather(binary_trait_data, trait, value, -(1:3))
    gr <- summarise(group_by(binary_trait_data_long, group2, trait),
                    n = length(na.omit(value)),
                    p = ifelse(n>0, mean(value > 0, na.rm=TRUE),0.5))
    Mn <- spread(gr[,c("group2","trait", "n")], trait, n)
    Mp <- spread(gr[,c("group2","trait", "p")], trait, p)
    
    list(Mn = Mn, Mp = Mp)
  }
  
  get_Mn_Mp_bothgroups <- function(binary_trait_data) {
    combinedGroup <- rep(NA, nrow(binary_trait_data))
    for (i in 1:nrow(binary_trait_data)){
      combinedGroup[i] <- paste0(binary_trait_data[i,]$GROUP1, "+", binary_trait_data[i,]$GROUP2)
    }
    binary_trait_data$combinedGroup <- combinedGroup
    binary_trait_data <- binary_trait_data[,c(1:3, ncol(binary_trait_data), 4:(ncol(binary_trait_data)-1))]
    colnames(binary_trait_data)[1:4] <- c("id", "group1", "group2", "bothgroups")
    binary_trait_data_long <- gather(binary_trait_data, trait, value, -(1:4))
    gr <- summarise(group_by(binary_trait_data_long, bothgroups, trait),
                    n = length(na.omit(value)),
                    p = ifelse(n>0, mean(value > 0, na.rm=TRUE),0.5))
    Mn <- spread(gr[,c("bothgroups","trait", "n")], trait, n)
    Mp <- spread(gr[,c("bothgroups","trait", "p")], trait, p)
    
    list(Mn = Mn, Mp = Mp)
  }
  # 
  # calculateMMD1 <- function(M_n, M_p, thetadiff, theta, deltamin = 0.01) {
  #   # remove traits with n == 0
  #   ind <- which(!apply(M_n == 0, 2, any))
  #   M_p <- M_p[,ind]
  #   M_n <- M_n[,ind]
  #   # end - remove traits with n == 0
  #   
  #   VarMatrix <- M_n[1:2, 2:length(M_n[1, ])]
  #   MMDMatrix <- matrix(0, length(M_n[, 1]), length(M_n[, 1]))
  #   
  #   for (a in seq_along(VarMatrix[1, ])) {
  #     for (b in seq_along(MMDMatrix[, 1])) {
  #       for (c in seq_along(MMDMatrix[1, ])) {
  #         tmp <- thetadiff(M_n[b,a+1], M_p[b,a+1], M_n[c,a+1], M_p[c,a+1], theta)
  #         tmp <- pmax(tmp, deltamin)
  #         MMDMatrix[b,c] <- tmp
  #       }
  #     }
  #     
  #     for (b in seq_along(MMDMatrix[, 1])) {
  #       for (c in seq_along(MMDMatrix[1, ])) {
  #         if (b >= c) {
  #           MMDMatrix[b, c] = 0
  #         }
  #       }
  #     }
  #     
  #     VNeg <- 0
  #     VPos <- 0
  #     for (b in seq_along(MMDMatrix[, 1])) {
  #       for (c in seq_along(MMDMatrix[1, ])) {
  #         if (MMDMatrix[b, c] > 0) {
  #           VPos = VPos + 1
  #         }
  #         if (MMDMatrix[b, c] < 0) {
  #           VNeg = VNeg + 1
  #         }
  #       }
  #     }
  #     
  #     VarMatrix[1, a] = sum(MMDMatrix)
  #     VarMatrix[2, a] = VPos / (VPos + VNeg)
  #   }
  #   
  #   VarStatus <- t(VarMatrix)
  #   
  #   ## -------------SECTION D: MMD MATRIX------------------------------------------
  #   
  #   MMDMatrix <- matrix(0, length(M_n[, 1]), length(M_n[, 1]))
  #   dimnames(MMDMatrix) <- list(M_n[, 1], M_n[, 1])
  #   
  #   for (a in seq_along(MMDMatrix[, 1])) {
  #     for (b in seq_along(MMDMatrix[1, ])) {
  #       MMDVect <- vector("double", length(M_n[1, ]) - 1)
  #       for (i in seq_along(MMDVect)) {
  #         tmp <- thetadiff(M_n[a,i+1], M_p[a,i+1], M_n[b,i+1], M_p[b,i+1], theta)
  #         tmp <- pmax(tmp, 0.01)
  #         MMDVect[i] <- tmp
  #       }
  #       MMDMatrix[a, b] <- sum(MMDVect) / length(MMDVect)
  #     }
  #   }
  #   
  #   ## forced 0 when a sample is compared to itself
  #   for (a in seq_along(MMDMatrix[,1])) { MMDMatrix[a,a] = 0 }
  #   
  #   ## -------------SECTION E: SD MATRIX-------------------------------------------
  #   
  #   ## standard deviation for MMD, Sjovold's formula
  #   SDMatrix <- matrix(0, length(M_n[, 1]), length(M_n[, 1]))
  #   dimnames(SDMatrix) <- list(M_n[, 1], M_n[, 1])
  #   SDDiff <- function(nA, nB) {
  #     (1 / nA + 1 / nB) ^ 2
  #   }
  #   
  #   for (a in seq_along(MMDMatrix[, 1])) {
  #     for (b in seq_along(MMDMatrix[1, ])) {
  #       SDVect <- vector("double", length(M_n[1, ]) - 1)
  #       for (i in seq_along(SDVect)) {
  #         SDVect[i] <- SDDiff(M_n[a, i + 1], M_n[b, i + 1])
  #       }
  #       SDMatrix[a, b] <- sqrt(sum(SDVect) * 2 / length(SDVect) ^ 2)
  #     }
  #   }
  #   
  #   ## -------------SECTION F: SIGNIFICANCE MATRIX---------------------------------
  #   
  #   ## statistical significance
  #   SigMatrix <- matrix(1, length(M_n[, 1]), length(M_n[, 1]))
  #   dimnames(SigMatrix) <- list(M_n[, 1], M_n[, 1])
  #   
  #   for (a in seq_along(MMDMatrix[, 1])) {
  #     for (b in seq_along(MMDMatrix[1, ])) {
  #       dist <- MMDMatrix[a, b] / SDMatrix[a, b]
  #       SigMatrix[a, b] = round((1 - pnorm(dist)) * 2, digits = 8)
  #       if (MMDMatrix[a, b] < 0)
  #         SigMatrix[a, b] = 1
  #     }
  #   }
  #   list(MMDMatrix = MMDMatrix, SDMatrix = SDMatrix, SigMatrix = SigMatrix)
  # }
  
  # getPCA <- function(df) {
  #   for (i in 4:ncol(df))
  #     df[,i] <- ifelse(is.na(df[,i]),
  #                      mean(df[,i], na.rm = TRUE),
  #                      df[,i])
  #   tmp <- df[,-(1:3)]
  #   inx <- apply(tmp,2, function(x) diff(range(x))) > 0
  #   tmp <- tmp[,which(inx)]
  #   
  #   ggbiplot(princomp(tmp), groups = df[,3], ellipse=TRUE) +
  #     ggtitle("PCA plot") + theme_classic() + xlab("") + ylab("")
  # }
  
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
