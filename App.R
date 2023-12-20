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
library(cluster)
library(ggrepel)
library(ggdendro)

source("functions.R")

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
                   selectInput("corMethod", label = "Inter-variable correlation test",
                               choices = c("Kendall" = "kendall", "Pearson" = "pearson", "Spearman" = "spearman")),
                   hidden(numericInput("corFlag", label = "Flag correlations at > ", value = 0.499))
                 ),
                 mainPanel = mainPanel(
                   tableOutput("descTable"),
                   downloadButton("downloaddDescTable", "Download Descriptives table"),
                   dataTableOutput("corTable"),
                   downloadButton("downloaddCorTable", "Download Correlation table")
                 )
               )
               
      ),
      tabPanel("Analysis - MMD",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("MMDFile", label = "Please upload a data file here", accept = c(".csv", ".rds", ".xlsx")),
                   selectInput("method_selMMD", label = "Method selection",
                               choices = c("MMD - Anscombe" = "MMD_ANS_0",
                                           "MMD - Freeman & Tukey" = "MMD_FRE_0",
                                           "MMD - Anscombe (Freeman & Tukey correction)" = "MMD_ANS_FRE",
                                           "MMD - Anscombe (Grewal correction)" = "MMD_ANS_GRE",
                                           "MMD - Freeman & Tukey (Freeman & Tukey correction)" = "MMD_FRE_FRE",
                                           "MMD - Freeman & Tukey (Grewal correction)" = "MMD_FRE_GRE"
                               ),
                               selected = "MMD_ANS_0"),
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
                                      ), selected = "ALL")
                 ),
                 mainPanel = mainPanel(
                   fluidRow(
                     column(width = 8,
                            plotOutput('ggMDSMMD', width = 600, height = 400),
                            downloadButton("downloadggMDSMMD", "Download MDS diagram"),
                            plotOutput('ggCzekanowskiMMD', width = 600, height = 400),
                            downloadButton("downloadggCzekanowskiMMD", "Download Czekanowski diagram"),
                            plotOutput('ggClustMMD', width = 600, height = 400),
                            downloadButton("downloadggClustMMD", "Download dendrogram"),
                            plotOutput('ggPCAMMD', width = 600, height = 400),
                            downloadButton("downloadggPCAMMD", "Download PCA plot"),
                     br(),
                     p("Distance matrix"),
                     tableOutput('distSummaryMMD'),
                     downloadButton("downloaddistSummaryMMD", "Download distance matrix"),
                     p("SD matrix"),
                     tableOutput('sdSummaryMMD'),
                     downloadButton("downloadsdSummaryMMD", "Download SD matrix"),
                     p("Significance matrix"),
                     tableOutput('signifSummaryMMD'),
                     downloadButton("downloadsignifSummaryMMD", "Download significance matrix")
                     ))
                   )
                 )
               
      ),
      tabPanel("Analysis - Mahalanobis",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("MahalanobisFile", label = "Please upload a data file here", accept = c(".csv", ".rds", ".xlsx")),
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
                   selectInput("groupChoiceMahalanobis", label = "Plots show",
                               choices = c("Group 1" = "group1",
                                           "Group 2" = "group2",
                                           "Group 1 + Group 2" = "bothgroups"),
                               selected = "group1"),
                   checkboxGroupInput("group_handling1Mahalanobis", label = "Group 1 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL"),
                   checkboxGroupInput("group_handling2Mahalanobis", label = "Group 2 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL")
                 ),
                 mainPanel = mainPanel(
                   fluidRow(
                     column(width = 8,
                            plotOutput('ggMDSMahalanobis', width = 600, height = 400),
                            downloadButton("downloadggMDSMahalanobis", "Download MDS diagram"),
                            plotOutput('ggCzekanowskiMahalanobis', width = 600, height = 400),
                            downloadButton("downloadggCzekanowskiMahalanobis", "Download Czekanowski diagram"),
                            plotOutput('ggClustMahalanobis', width = 600, height = 400),
                            downloadButton("downloadggClustMahalanobis", "Download Dendrogram"),
                            plotOutput('ggPCAMahalanobis', width = 600, height = 400),
                            downloadButton("downloadggPCAMahalanobis", "Download PCA plot"),
                            br(),
                            p("Distance matrix"),
                            tableOutput('distSummaryMahalanobis'),
                            downloadButton("downloaddistSummaryMahalanobis", "Download distance matrix"),
                            p("SD matrix"),
                            tableOutput('sdSummaryMahalanobis'),
                            downloadButton("downloadsdSummaryMahalanobis", "Download SD matrix"),
                            p("Significance matrix"),
                            tableOutput('signifSummaryMahalanobis'),
                            downloadButton("downloadsignifSummaryMahalanobis", "Download significance matrix")
                     ))
                 )
               )
               
      ),
      tabPanel("Analysis - Gower",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("GowerFile", label = "Please upload a data file here", accept = c(".csv", ".rds", ".xlsx")),
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
                   selectInput("groupChoiceGower", label = "Plots show",
                               choices = c("Group 1" = "group1",
                                           "Group 2" = "group2",
                                           "Group 1 + Group 2" = "bothgroups"),
                               selected = "group1"),
                   checkboxGroupInput("group_handling1Gower", label = "Group 1 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL"),
                   checkboxGroupInput("group_handling2Gower", label = "Group 2 handling",
                                      choices = c("All individuals" = "ALL"
                                      ), selected = "ALL")
                 ),
                 mainPanel = mainPanel(
                   fluidRow(
                     column(width = 8,
                            plotOutput('ggMDSGower', width = 600, height = 400),
                            downloadButton("downloadggMDSGower", "Download MDS diagram"),
                            plotOutput('ggClustGower', width = 600, height = 400),
                            downloadButton("downloadggClustGower", "Download Dendrogram"),
                            br(),
                            p("Distance matrix"),
                            tableOutput('distSummaryGower'),
                            downloadButton("downloaddistSummaryGower", "Download distance matrix"),
                            p("PermDisp Test"),
                            textOutput("permDispGower"),
                            downloadButton("permDispGowerPDF", "Download perm disp"),
                            p("PermAnova Test"),
                            textOutput("pairwiseAdonisGower"),
                            downloadButton("pairwiseAdonisGowerPDF", "Download pairwise adonis")
                     ))
                 )
               )
               
      ),
      tabPanel("Help",
      )
    )
  )
)


server <- function(input, output, session) {
  
  v <- reactiveValues(descUpload = NULL)
  
  
  observeEvent(input$descriptivesFile,{
    v$descUpload <- "yes"
  })
  
  
  inputDescData <- reactive({
    
    
    if (is.null(v$descUpload)){
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
  

  
  
  doCorrelation <- function(){
    
    myData <- inputDescData()$descUpload
    
    if (is.null(myData)) return(NULL)
    
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
    
  }
  
  
  
  doDescriptives <- function(){
    
    
    myData <- inputDescData()$descUpload
    
    if (is.null(myData)) return(NULL)
    
    if (input$groupTrait=="nogroup"){

      thresholdValues <- myData[1,]
      
      myData <- myData[-1,]
      
      withProgress(message = 'Rendering output', value = 0, {
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
      
      
    }
    
    if (input$groupTrait=="group 1"){
      
      
      thresholdValues <- myData[1,]
      
      myData <- myData[-1,]
      
      withProgress(message = 'Rendering output', value = 0, {
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
      
      inputDescData()$descUpload
      
      thresholdValues <- myData[1,]
      
      myData <- myData[-1,]
      
      withProgress(message = 'Rendering output', value = 0, {
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
      
      thresholdValues <- myData[1,]
      
      myData <- myData[-1,]
      
      
      combinedGroup <- rep(NA, nrow(myData))
      
      for (i in 1:nrow(myData)){
        combinedGroup[i] <- paste0(myData[i,]$GROUP1, "+", myData[i,]$GROUP2)
      }
      
      myData$combinedGroup <- combinedGroup
      
      myData <- myData[,c(1:3, ncol(myData), 4:(ncol(myData)-1))]
      
      withProgress(message = 'Rendering output', value = 0, {
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
    
    
  }
            
  desc_table <- function() {
    di <- doDescriptives()$finalData
    if (is.null(di)) "Upload data" else di
  }
  
  # cor_table <- function() {
  # 
  #   rowCallback <- c(
  #     "function(row, data){",
  #     "  for(var i=0; i<data.length; i++){",
  #     "    if(data[i] === null){",
  #     "      $('td:eq('+i+')', row).html('NA')",
  #     "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  #     "    }",
  #     "  }",
  #     "}"
  #   )
  # 
  #   di <-  doCorrelation()$corMatrix
  # 
  #   if (is.null(di)) {
  #     x <- data.frame("Upload data")
  #     colnames(x) <- "Please upload some data"
  #     x
  #   } else {
  # 
  # 
  #     x <- datatable(data, options = list(rowCallback = JS(rowCallback))) %>% formatStyle(
  #       columns = colnames(data),
  #       backgroundColor = styleInterval(c(input$corFlag, 0.999, 1.001, 1000), c('white', 'lightgreen', 'lightblue', 'lightyellow', 'red'))
  #     ) %>%
  #       formatSignif(
  #         columns = colnames(data),
  #         digits = 3
  #       )
  #     x
  #   }
  # }
  
  
  
  output$descTable <- renderTable({
    desc_table()
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
    
    if (is.null(data)){
      x <- data.frame("Upload some data")
      colnames(x) <- "Please upload some data"
      x
    } else {
    
    
    datatable(data, options = list(rowCallback = JS(rowCallback))) %>% formatStyle(
      columns = colnames(data),
      backgroundColor = styleInterval(c(input$corFlag, 0.999, 1.001, 1000), c('white', 'lightgreen', 'lightblue', 'lightyellow', 'red'))
    ) %>%
      formatSignif(
        columns = colnames(data),
        digits = 3
      )
      
    }
    
  
    
  })
  
  output$downloaddDescTable <- downloadHandler(
    filename = "DescTable.csv",
    content = function(file) {
      write.csv(desc_table(), file, row.names = FALSE)
    }
  )
  
  output$downloaddCorTable <- downloadHandler(
    filename = "corTable.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "corTable.Rmd")
      file.copy("corTable.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(corTableOutput =  doCorrelation()$corMatrix, corFlag = input$corFlag)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  

  observe({
    
    inFile <- input$MMDFile
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
    
    inFile <- input$MMDFile
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
    inFile <- input$MMDFile
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
    }
    
    # side handling
    if (input$init_traitMMD == "RIGHT") {
      df <- df[,c(1:3,seq(5, ncol(df), 2))]
    }
    if (input$init_traitMMD == "LEFT") {
      df <- df[,c(1:3,seq(4, ncol(df), 2))]
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
      
      
      temp <- tmp
      ind <- which(!apply(temp$Mn == 0, 2, any))
      temp$Mn <- temp$Mn[,ind]
      
      if(dim(temp$Mn)[2]>2){
        res <- dentalAffinities::calculateMMD(data.frame(tmp$Mn), as.data.frame(tmp$Mp), thetadiff, theta)
      }
    }
    if (selected[1] == "MAH") {
      res <- dentalAffinities::calculateD2(df)
    }
    res
  })
  
  # table
  
  ggMDSMMD_plot <- function(){
    di <- dataInputMMD()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMMD()
    if (is.null(mat)){
      return(grid::grid.text('Too much missing data, try removing some variables in the chosen group'))
    } else {
      getMDS(mat$MMDMatrix)
    }
    
  }
  
  ggCzekanowskiMMD_plot <- function(){
    di <- dataInputMMD()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMMD()
    if (is.null(mat)){
      return(grid::grid.text('Too much missing data, try removing some variables in the chosen group'))
    } else {
      dentalAffinities::getCzekanowski(mat$MMDMatrix)
    }
  }
  
  ggPCAMMD_plot <- function(){
    di <- dataInputMMD()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    dentalAffinities::getPCA(di)
  }
  
  ggClustMMD_plot <- function(){
    di <- dataInputMMD()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMMD()
    if (is.null(mat)){
      return(grid::grid.text('Too much missing data, try removing some variables in the chosen group'))
    } else {
      getClust(mat$MMDMatrix)
    }
  }
  
  distSummaryMMD_table <- function(){
    di <- dataInputMMD()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMMD()$MMDMatrix
      if (is.null(mat)) {
        print("MMD matrix is not available")
      } else {
        round(mat, 2)
      }    }
  }
  
  sdSummaryMMD_table <- function(){
    di <- dataInputMMD()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMMD()$SDMatrix
      if (is.null(mat)) {
        print("SD matrix is not available")
      } else {
        round(mat, 2)
      }
    }
  }
  
  signifSummaryMMD_table <- function(){
    di <- dataInputMMD()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMMD()$SigMatrix
      if (is.null(mat)) {
        print("P-values matrix is not available")
      } else {
        round(mat, 5)
      }
    }
  }
                                    
  
  output$ggMDSMMD <- renderPlot({
    ggMDSMMD_plot()
  })
  
  output$ggCzekanowskiMMD <- renderPlot({
    ggCzekanowskiMMD_plot()
  })
  
  output$ggPCAMMD <- renderPlot({
    ggPCAMMD_plot()
  })
  
  output$ggClustMMD <- renderPlot({
    ggClustMMD_plot()
  })
  
  output$distSummaryMMD <- renderTable({
    distSummaryMMD_table()
  })
  output$sdSummaryMMD <- renderTable({
    sdSummaryMMD_table()
  })
  
  output$signifSummaryMMD <- renderTable({
    signifSummaryMMD_table()
  })
  
  output$downloadggMDSMMD <- downloadHandler(
    filename = "ggMDSMMD.png",
    content = function(file) {
      ggsave(file, ggMDSMMD_plot(), device = png)
    }
  )
  
  output$downloadggCzekanowskiMMD <- downloadHandler(
    filename = "ggCzekanowskiMMD.png",
    content = function(file) {
      ggsave(file, ggCzekanowskiMMD_plot(), device = png)
    }
  )
  
  output$downloadggPCAMMD <- downloadHandler(
    filename = "ggPCAMMD.png",
    content = function(file) {
      ggsave(file, ggPCAMMD_plot(), device = png)
    }
  )
  
  output$downloadggClustMMD <- downloadHandler(
    filename = "ggClustMMD.png",
    content = function(file) {
      ggsave(file, ggClustMMD_plot(), device = png)
    }
  )
  
  output$downloaddistSummaryMMD <- downloadHandler(
    filename = "distSummaryMMD.csv",
    content = function(file) {
      write.csv(distSummaryMMD_table(), file, row.names = FALSE)
    }
  )
  
  output$downloadsdSummaryMMD <- downloadHandler(
    filename = "sdSummaryMMD.csv",
    content = function(file) {
      write.csv(sdSummaryMMD_table(), file, row.names = FALSE)
    }
  )
  
  output$downloadsignifSummaryMMD <- downloadHandler(
    filename = "signifSummaryMMD.csv",
    content = function(file) {
      write.csv(signifSummaryMMD_table(), file, row.names = FALSE)
    }
  )
  
  
  #Mahalanobis functions
  
  observe({
    
    inFile <- input$MahalanobisFile
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
    
    inFile <- input$MahalanobisFile
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
    inFile <- input$MahalanobisFile
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
    }
    
    
    # side handling
    if (input$init_traitMahalanobis == "RIGHT") {
      df <- df[,c(1:3,seq(5, ncol(df), 2))]
    }
    if (input$init_traitMahalanobis == "LEFT") {
      df <- df[,c(1:3,seq(4, ncol(df), 2))]
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
      
      if (input$groupChoiceMahalanobis=="group1"){tmp <- get_Mn_Mp_group_1(df)}
      if (input$groupChoiceMahalanobis=="group2"){tmp <- get_Mn_Mp_group_2(df)}
      if (input$groupChoiceMahalanobis=="bothgroups"){tmp <- get_Mn_Mp_bothgroups(df)}
      
      
      temp <- tmp
      ind <- which(!apply(temp$Mn == 0, 2, any))
      temp$Mn <- temp$Mn[,ind]
      
      if(dim(temp$Mn)[2]>2){
        res <- dentalAffinities::calculateMMD(data.frame(tmp$Mn), as.data.frame(tmp$Mp), thetadiff, theta)
      }
      
      res <- dentalAffinities::calculateMMD(data.frame(tmp$Mn), as.data.frame(tmp$Mp), thetadiff, theta)
    }
    if (selected[1] == "MAH") {
      res <- dentalAffinities::calculateD2(df)
    }
    res
  })
  
  # table
  
  ggMDSMahalanobis_plot <- function(){
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMahalanobis()
    getMDS(mat$MMDMatrix)
  }
  
  ggCzekanowskiMahalanobis_plot <- function(){
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMahalanobis()
    dentalAffinities::getCzekanowski(mat$MMDMatrix)
  }
  
  ggPCAMahalanobis_plot <- function(){
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    dentalAffinities::getPCA(di)
  }
  
  ggClustMahalanobis_plot <- function(){
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDistMahalanobis()
    getClust(mat$MMDMatrix)
  }
  
  distSummaryMahalanobis_table <- function(){
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMahalanobis()$MMDMatrix
      if (is.null(mat)){
        print("MMD matrix is not available")
      } else {
      round(mat, 2)
    }
    }
  }
  
  sdSummaryMahalanobis_table <- function(){
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMahalanobis()$SDMatrix
      if (is.null(mat)) {
        "SD matrix is not available"
      } else {
        round(mat, 2)
      }
    }
  }
  
  signifSummaryMahalanobis_table <- function(){
    di <- dataInputMahalanobis()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDistMahalanobis()$SigMatrix
      if (is.null(mat)) {
        "P-values matrix is not available"
      } else {
        round(mat, 5)
      }
    }
  }
  
  
  output$ggMDSMahalanobis <- renderPlot({
    ggMDSMahalanobis_plot()
  })

  output$ggCzekanowskiMahalanobis <- renderPlot({
    ggCzekanowskiMahalanobis_plot()
  })
  
  output$ggPCAMahalanobis <- renderPlot({
    ggPCAMahalanobis_plot()
  })
  
  # table
  output$ggClustMahalanobis <- renderPlot({
    ggClustMahalanobis_plot()
  })
  
  output$distSummaryMahalanobis <- renderTable({
    distSummaryMahalanobis_table()
  })
  
  output$sdSummaryMahalanobis <- renderTable({
    sdSummaryMahalanobis_table()
  })
  
  output$signifSummaryMahalanobis <- renderTable({
    signifSummaryMahalanobis_table()
  })
  
  output$downloadggMDSMahalanobis <- downloadHandler(
    filename = "ggMDSMahalanobis.png",
    content = function(file) {
      ggsave(file, ggMDSMahalanobis_plot(), device = png)
    }
  )
  
  output$downloadggCzekanowskiMahalanobis <- downloadHandler(
    filename = "ggCzekanowskiMahalanobis.png",
    content = function(file) {
      ggsave(file, ggCzekanowskiMahalanobis_plot(), device = png)
    }
  )
  
  output$downloadggPCAMahalanobis <- downloadHandler(
    filename = "ggPCAMahalanobis.png",
    content = function(file) {
      ggsave(file, ggPCAMahalanobis_plot(), device = png)
    }
  )
  
  output$downloadggClustMahalanobis <- downloadHandler(
    filename = "ggClustMahalanobis.png",
    content = function(file) {
      ggsave(file, ggClustMahalanobis_plot(), device = png)
    }
  )
  
  output$downloaddistSummaryMahalanobis <- downloadHandler(
    filename = "distSummaryMahalanobis.csv",
    content = function(file) {
      write.csv(distSummaryMahalanobis_table(), file, row.names = FALSE)
    }
  )
  
  output$downloadsdSummaryMahalanobis <- downloadHandler(
    filename = "sdSummaryMahalanobis.csv",
    content = function(file) {
      write.csv(sdSummaryMahalanobis_table(), file, row.names = FALSE)
    }
  )
  
  output$downloadsignifSummaryMahalanobis <- downloadHandler(
    filename = "signifSummaryMahalanobis.csv",
    content = function(file) {
      write.csv(signifSummaryMahalanobis_table(), file, row.names = FALSE)
    }
  )
  

  
  
  
  #Gower functions

  observe({
    
    inFile <- input$GowerFile
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
    
    inFile <- input$GowerFile
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
    inFile <- input$GowerFile
    if (is.null(inFile)) {
      return(NULL)
    } else {
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
    }
    
    
    # side handling
    if (input$init_traitGower == "RIGHT") {
      df <- df[,c(1:3,seq(5, ncol(df), 2))]
    }
    if (input$init_traitGower == "LEFT") {
      df <- df[,c(1:3,seq(4, ncol(df), 2))]
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
    
    df<-df[which(rowMeans(!is.na(df)) > 0.7), ]
    
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
  
  
  #Functions to create the plots/tables
  ggMDSGower_plot <- function(){
    di <- dataInputGower()
    if (is.null(di)) {
      return(NULL)
    }
    set.seed(1234)
    y <- vegdist(di[,4:ncol(di)], method="gower", na.rm = T)
    betadisper(y, group = di$GROUP1, add = T)
  }
  
  #dendrogram Gower
  
  ggClustGower_plot <- function(){
    di <- dataInputGower() 
    
    if (is.null(di)) {
      return(NULL)
    }
    set.seed(1234)
    y <- vegdist(di[,4:ncol(di)], method="gower", na.rm = T)
    getClust(y)
  }
  
  
  distSummaryGower_table <- function(){
    di <- dataInputGower()
    if (is.null(di)) {
      return(NULL)
    }
    set.seed(1234)
    y <- vegdist(di[,4:ncol(di)], method="gower", na.rm = T)
    x <- as.data.frame(as.matrix(y))
    x
  }
  
  permDispGower_text <- function(){
    di <- dataInputGower()
    if (is.null(di)) {
      return(NULL)
    }
    set.seed(1234)
    y <- vegdist(di[,4:ncol(di)], method="gower", na.rm = T)
    y
  }
  
  pairwiseAdonisGower_text <- function(){
    di <- dataInputGower()
    if (is.null(di)) {
      return(NULL)
    }
    set.seed(1234)
    y <- vegdist(di[,4:ncol(di)], method="gower", na.rm = T)
    pairwise.adonis2(y~GROUP1, data = di, p.adjust = "holm")
  }
  
  #Functions to output the plots/tables
  output$ggMDSGower <- renderPlot({
    if (is.null(ggMDSGower_plot())){
      grid::grid.text('Please, first upload a file with data')
    } else {
      plot(ggMDSGower_plot())
    }
  })
  
  output$ggClustGower <- renderPlot({
    if (is.null(ggClustGower_plot())){
      grid::grid.text('Please, first upload a file with data')
    } else {
      plot(ggClustGower_plot())
    }
  })
  
  output$distSummaryGower <- renderText({
    if (is.null(distSummaryGower_table())){
      'Please, first upload a file with data'
    } else {
      'This has ran correctly, please download the output (below)'
    }
  })
  
  
  output$permDispGower <- renderText({
    if (is.null(permDispGower_text())){
      'Please, first upload a file with data'
    } else {
      'This has ran correctly, please download the output (below)'
    }
  })
  
  output$pairwiseAdonisGower <- renderText({
    if (is.null(pairwiseAdonisGower_text())){
      'Please, first upload a file with data'
    } else {
      'This has ran correctly, please download the output (below)'
    }
  })
  
  #Functions to download the plots/tables
  
  output$downloadggMDSGower <- downloadHandler(
    file = "ggMDSGower.png" , # variable with filename
    content = function(file) {
      #ggsave(p(), filename = file)
      png(file = file)
      plot(ggMDSGower_plot())
      dev.off()
    })
  
  output$downloadggClustGower <- downloadHandler(
    file = "ggClustGower.png" , # variable with filename
    content = function(file) {
      #ggsave(p(), filename = file)
      png(file = file)
      plot(ggClustGower_plot())
      dev.off()
    })

  
  
  output$downloaddistSummaryGower <- downloadHandler(
    filename = "distSummaryGower.csv",
    content = function(file) {
      write.csv(distSummaryGower_table(), file, row.names = FALSE)
    }
  )

  
  
  output$permDispGowerPDF <- downloadHandler(
    filename = "permDispGower.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "permDispGower.Rmd")
      file.copy("permDispGower.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(permDispGowerOutput = ggMDSGower_plot())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  output$pairwiseAdonisGowerPDF <- downloadHandler(
    filename = "pairwiseAdonisGower.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "pairwiseAdonisGower.Rmd")
      file.copy("pairwiseAdonisGower.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(pairwiseAdonisGowerOutput = pairwiseAdonisGower_text())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  output$percentMD <- renderUI({

    df <- dataInputGower()
    #Percentage of missing values in the data set
    paste0("The percentage of missing data in your data set is ", round(sum(is.na(df[,4:ncol(df)]))/
      prod(dim(df[,4:ncol(df)])), 2)*100, "%")

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


  
}

shinyApp(ui, server)
