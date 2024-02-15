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
library(viridis)
library(ggplot2)

source("functions.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Dental Affinities"),
  mainPanel(
    
    tabsetPanel(
      
      #Descriptives UI ---------------------------------
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
      #MMD Analysis UI ---------------------------------
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
      #Mahalanobis Analysis UI ---------------------------------
      tabPanel("Analysis - Mahalanobis",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("MahalanobisFile", label = "Please upload a data file here", accept = c(".csv", ".rds", ".xlsx")),
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
      #Gower Analysis UI ---------------------------------
      tabPanel("Analysis - Gower",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("GowerFile", label = "Please upload a data file here", accept = c(".csv", ".rds", ".xlsx")),
                   numericInput("gowerMissingData", label = "Delete rows with < missing data", value = 0.7),
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
      #Help UI ---------------------------------
      tabPanel("Help",
               HTML("<p><u>Overview</u></p>"),
               HTML("<p>dentalAffinities provides tools for analysing non-metric data using Mean Measure of Divergence, Mahalanobis D 2 and Gower coefficients. While we do our best to ensure that the software provides theoretically grounded and accurate results, 
                    we do not hold responsibility for its (mis)use. This is a free tool to facilitate statistical analysis, users are expected to have the required expertise and background for evaluating the appropriateness of their analysis and results.</p>"),
               HTML("<p>Cite this as: ?</p>"),
               HTML("<p><u>Template file</u></p>"),
               HTML("<p>We provide a template file which must be used with the app.</p>"),
               downloadButton("downloadTemplate", label = "Download template data"),
               HTML("<p>This file is for raw, un-pooled data; the app has been designed to dichotomise/binarize and group the results for you.</p>"),
               HTML("<p>Do not change the column names for SKELETON, GROUP1, GROUP2, but you may use any name/identifier under these columns. GROUP1 and GROUP2 have been provided to give more flexibility for the user to choose between multiple classifiers or their 
                    combination – as an example, GROUP1 could be site data and GROUP2 could be sex.</p>"),
               HTML("<p>You may change the number and name of traits to whichever you choose.</p>"),
               HTML("<p>Do not alter the cell THRESHOLD. You may change the threshold numbers/breakpoints for each traits to whatever you choose: the value must be placed in the cell below each trait.</p>"),
               HTML("<p><u>Descriptives</u></p>"),
               HTML("<p>The descriptives have been designed to offer a basic frequency (n) table of your data by any grouping of your choosing; you can either choose to group by one variable in either GROUP1 or GROUP2 or a combination of both, i.e. GROUP1+GROUP2.</p>"),
               HTML("<p>You may also choose to test inter-trait correlation of your sample; this analysis pools all data and generates a correlation matrix of the raw values. The lower diagonal shows the number of observations, the upper diagonal shows the correlation value.</p>"),
               HTML("<p><u>Mean Measure of Divergence (MMD)</u></p>"),
               HTML("<p>MMD calculates distances between groups by using the number of positive and overall observations of traits together, combining the individual trait distances into one overall value between the groups. An increase in the distance value indicates increasing distance or
                    dissimilarity between groups. The app uses a modified version of <span style='color: blue;'>Smith’s (1972)</span> MMD as described by <span style='color: blue;'>Sołtysiak (2011)</span>. Variables that produce negative distance are corrected to zero.</p>"),
               HTML("<p>You may choose to use MMD with either Anscombe or Freeman and Tukey angular transformation, with an additional optional small sample size correction with either Freeman and Tukey or Grewal as described in <span style='color: blue;'>Harris and Sjøvold (2004)</span>. There are further pre-analysis options:</p>
<ul>
  <li><b>Initial trait selection</b>: All traits is the default option, but in case you have organised traits by side (left and right), you may 1) choose a side, 2) tell the app to choose the higher or lower score, or 3) generate an average of left and right. If you decide to use any of these three options, the app assumes each variable/column
  is paired left and right, in this order (left on the left, right on the right) and select accordingly. Please note that the correlation test does not have this option available.
  </li>
  <li><b>Binarization</b>: The raw data can be dichotomised either by using 1) user-defined values (from the THRESHOLD row of the file), 2) balanced, or 3) highest X 2 values.
  </li>
  <li><b>Plots show</b>: Offers an option to use different (GROUP1 or GROUP2) or combined (GROUP1+GROUP2) grouping variables. Under the drop-down menu, options become available to select/de-select group variables, initiating a real-time update in the results.</li>
</ul>"),
               HTML("<p><u>Results</u></p>"),
               HTML("<p><b>MDS diagram</b>. Calls isoMDS, a non-metric multidimensional tool for visualising non-parametric data, from package MASS. The plot is produced using ggplot <span style='color: blue;'>(Wickham 2016)</span>.</p>"),
               HTML("<p><b>Czekanowski diagram</b>. Based on Czekanowski diagrams as described in http://antropologia.uw.edu.pl/MaCzek/maczek.html. The size of the squares visualises the proximity/similarity of the groups. The plot is produced using ggplot <span style='color: blue;'>(Wickham 2016)</span>.</p>"),
               HTML("<p><b>Dendrogram</b>. The app calls the function agnes from the package cluster <span style='color: blue;'>(Maechler et al 2022)</span>, using Ward’s method which calculates the sum of the square of the distances. Results are plotted using function ggdendrogram from the package dendextend <span style='color: blue;'>(Galili et al 2023)</span>.</p>"),
               HTML("<p><b>PCA plot</b>. Using the princomp function from base R, the app performs a principal component analysis on the raw data and plots the eigenvectors for each variable. The plot is produced using ggbiplot <span style='color: blue;'>(Vu and Friendly 2023)</span>.</p>"),
               HTML("<p><b>Matrices</b>. The app produces a distance, SD and significance matrix that can be dowloaded as Comma Separated Values Excel file.</p>"),
               HTML("<p><u>Mahalanobis D2</u></p>"),
               HTML("<p> D2 calculates weighted average correlations for each sample using binary data. It can adjust for small sample sizes and account for any residual inter-trait correlation but it cannot cope with missing data; thus, the app removes missing data from the matrix. The code is based 
                    on Lyle W. Konigsberg’s scripts (tdistR.zip) http://lylek.ucoz.org/index.html with some additional cleaning and regularisation. Variables that produce negative distance are corrected to zero.</p>"),
               HTML("<p>There are some pre-analysis options:</p>
<ul>
  <li><b>Initial trait selection</b>: All traits is the default option, but in case you have organised traits by side (left and right), you may 1) choose a side, 2) tell the app to choose the higher or lower score, or 3) generate an average of left and right. If you decide to use any of these three options, the app assumes each variable/column
  is paired left and right, in this order (left on the left, right on the right) and select accordingly. Please note that the correlation test does not have this option available.
  </li>
  <li><b>Binarization</b>: The raw data can be dichotomised either by using 1) user-defined values (from the THRESHOLD row of the file), 2) balanced, or 3) highest X 2 values.
  </li>
  <li><b>Plots show</b>: Offers an option to use different (GROUP1 or GROUP2) or combined (GROUP1+GROUP2) grouping variables. Under the drop-down menu, options become available to select/de-select group variables, initiating a real-time update in the results.</li>
</ul>"),
               HTML("<p><u>Results</u></p>"),
               HTML("<p><b>MDS diagram</b>. Calls isoMDS, a non-metric multidimensional tool for visualising non-parametric data, from package MASS. The plot is produced using ggplot <span style='color: blue;'>(Wickham 2016)</span>.</p>"),
               HTML("<p><b>Czekanowski diagram</b>. Based on Czekanowski diagrams as described in http://antropologia.uw.edu.pl/MaCzek/maczek.html. The size of the squares visualises the proximity/similarity of the groups. The plot is produced using ggplot <span style='color: blue;'>(Wickham 2016)</span>.</p>"),
               HTML("<p><b>Dendrogram</b>. The app calls the function agnes from the package cluster <span style='color: blue;'>(Maechler et al 2022)</span>, using Ward’s method which calculates the sum of the square of the distances. Results are plotted using function ggdendrogram from the package dendextend <span style='color: blue;'>(Galili et al 2023)</span>.</p>"),
               HTML("<p><b>PCA plot</b>. Using the princomp function from base R, the app performs a principal component analysis on the raw data and plots the eigenvectors for each variable. The plot is produced using ggbiplot <span style='color: blue;'>(Vu and Friendly 2023)</span>.</p>"),
               HTML("<p><b>Matrices</b>. The app produces a distance, SD and significance matrix that can be dowloaded as Comma Separated Values Excel file.</p>"),
               HTML("<p><u>Gower</u></p>"),
               HTML("<p>Gower coefficients measure the difference between observations, beginning by computing the distance between pairs and then combining the distances into a single value per record-pair <span style='color: blue;'>(Gower 1971)</span>. The Gower distance matrix was generated using the function xx. For an automated output, 
                    the test is performed on dichotomised values.</p>"),
               HTML("<p>There are some pre-analysis options:</p>
<ul>
  <li><b>Initial trait selection</b>: All traits is the default option, but in case you have organised traits by side (left and right), you may 1) choose a side, 2) tell the app to choose the higher or lower score, or 3) generate an average of left and right. If you decide to use any of these three options, the app assumes each variable/column
  is paired left and right, in this order (left on the left, right on the right) and select accordingly. Please note that the correlation test does not have this option available.
  </li>
  <li><b>Binarization</b>: The raw data can be dichotomised either by using 1) user-defined values (from the THRESHOLD row of the file), 2) balanced, or 3) highest X 2 values.
  </li>
  <li><b>Plots show</b>: Offers an option to use different (GROUP1 or GROUP2) or combined (GROUP1+GROUP2) grouping variables. Under the drop-down menu, options become available to select/de-select group variables, initiating a real-time update in the results.</li>
</ul>"),
               HTML("<p><u>Results</u></p>"),
               HTML("<p><b>PCoA plot</b>. Principal Coordinate Analysis draws from the output of xx.</p>"),
               HTML("<p><b>Dendrogram</b>. The app calls the function agnes from the package cluster <span style='color: blue;'>(Maechler et al 2022)</span>, using Ward’s method which calculates the sum of the square of the distances. Results are plotted using function ggdendrogram from the package dendextend <span style='color: blue;'>(Galili et al 2023)</span>.</p>"),
               HTML("<p><b>Distance matrix</b>. As Gower distance analysis generates distances for each row, the file is potentially too large to display in the app but it can be dowloaded as a as Comma Separated Values Excel file.</p>"),
               HTML("<p><b>Perm Disp</b>. The app calls for the function betadisper in the package vegan <span style='color: blue;'>(Oksanen et al 2016)</span>. This test measures the multivariate homogeneity of groups dispersions (variances) by reducing the original distances to principal coordinates. This file can be dowloaded as an HTML file.</p>"),
               HTML("<p><b>PermANOVA Test</b>. The app calls for the function pairwise.adonis2 <span style='color: blue;'>(Martinez Arbizu 2020)</span>, a wrapper function for the function adonis2 from package vegan. This is a multi-level permutational ANOVA test that can detect differences in group mean location (or direction) and group dispersion (spread). 
                    This file can be dowloaded as an HTML file.</p>"),
               HTML("<p><u>References</u></p>"),
               HTML("<p>Galili, T. et al. 2023. dendextend: Extending &#39;dendrogram&#39; Functionality in R. https://CRAN.R-project.org/package=dendextend.</p>"),
               HTML("<p>Gower, J. C. 1971. A General Coefficient of Similarity and Some of Its Properties. Biometrics, 27 (4),857-871.</p>"),
               HTML("<p>Harris, E. F., Sjøvold, T. 2004. Calculation of Smith’s mean measure of divergence for intergroup comparisons using nonmetric data. Dental Anthropology, (17), 83-93.</p>"),
               HTML("<p>Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., Hornik, K. 2022. cluster: Cluster Analysis Basics and Extensions. https://CRAN.R-project.org/package=cluster.</p>"),
               HTML("<p>Martinez Arbizu, P. 2020. pairwiseAdonis: Pairwise multilevel comparison using adonis. R package version 0.4, https://github.com/pmartinezarbizu/pairwiseAdonis.</p>"),
               HTML("<p>Oksanen, J. et al 2022. vegan: Community Ecology Package_. R package version 2.6-4, https://CRAN.R-project.org/package=vegan.</p>"),
               HTML("<p>Smith, C. A. B. 1972. Coefficients of biological distance. Annals of Human Genetics, (36), 241-245.</p>"),
               HTML("<p>Sołtysiak, A. 2011. An R script for Smith’s mean measure of divergence. Bioarchaeology of the Near East 5, 21–44.</p>"),
               HTML("<p>Vu, V., Friendly, M. 2023. ggbiplot: A Grammar of Graphics Implementation of Biplots_. R package version 0.6.1, https://CRAN.R-project.org/package=ggbiplot.</p>"),
               HTML("<p>Wickham, H. 2016. ggplot2: Elegant Graphics for Data Analysis. New York: Springer-Verlag.</p>")
               )
    )
  )
)


server <- function(input, output, session) {
  
  #Descriptives Logic ---------------------------------
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
            count(.[[i]]) 
          
          colnames(myData1) <- c(colnames(myData)[i], "n")
          
          myData1[2] <- as.numeric(unlist(myData1[2]))
          
          
          uniquescores <- myData1[1] %>%
            unique() %>%
            unlist() %>%
            sort()
          
          newData <- as.data.frame(matrix(ncol = 3, nrow = length(uniquescores)))
          colnames(newData) <- c("Trait", "Score", "Count")
          

          
          for (j in 1:length(uniquescores)){
            newData$Trait[j] <- as.character(colnames(myData)[i])
            newData$Score[j] <- uniquescores[j]
          }
          

            for (k in 1:length(uniquescores)){
              newData[k,3] <- myData1[k,2]
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
  
  
  #MMD Logic ---------------------------------
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
  
  
  #Mahalanobis Logic ---------------------------------
  
  
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
    
      if (input$groupChoiceMahalanobis=="group1"){res <- calculateD2_group1(df)}
      if (input$groupChoiceMahalanobis=="group2"){res <- calculateD2_group2(df)}
      if (input$groupChoiceMahalanobis=="bothgroups"){res <- calculateD2_bothgroups(df)}
    
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
  
  #Gower Logic ---------------------------------
  
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
    
    df<-df[which(rowMeans(!is.na(df)) > input$gowerMissingData), ]
    
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
  
  

  ggMDSGower_plot <- function(){
    di <- dataInputGower()
    
    if (is.null(di)) {
      return(NULL)
    }
    set.seed(1234)
    y <- vegdist(di[,4:ncol(di)], method="gower", na.rm = TRUE)
    mds <- cmdscale(y)
    
    combinedGroup <- rep(NA, nrow(di))
    for (i in 1:nrow(di)){
      combinedGroup[i] <- paste0(di[i,2], "+", di[i,3])
    }
    di$combinedGroup <- combinedGroup
    di <- di[,c(1:3, ncol(di), 4:(ncol(di)-1))]
    colnames(di)[1:4] <- c("id", "group1", "group2", "bothgroups")
    
    # Convert MDS result to data frame
    
    
    if (input$groupChoiceGower=="group1"){
      mds_df <- data.frame(x = mds[,1], y = mds[,2], group = di$group1)
    } 
    
    if (input$groupChoiceGower=="group2"){
      mds_df <- data.frame(x = mds[,1], y = mds[,2], group = di$group2)
    } 
    
    if (input$groupChoiceGower=="bothgroups"){
      mds_df <- data.frame(x = mds[,1], y = mds[,2], group = di$bothgroups)
    } 
    
    
    p <- ggplot(mds_df, aes(x, y, color = group)) +
      geom_point() +
      labs(x = "PCoA 1", y = "PCoA 2") +
      theme(legend.position="top") +
      theme(legend.title = element_blank()) +
      stat_ellipse(level = 0.95) +
      guides(shape=guide_legend(nrow=1)) +
      guides(size = FALSE) +
      theme(legend.background = element_rect(fill = "white"),
            legend.key = element_rect(fill = "white", color = NA))+
      theme(panel.border = element_rect(linetype = "solid", fill = NA),
            panel.background = element_rect(fill = "white")) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
    
    print(p)
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
    y <- vegdist(di[,4:ncol(di)], method="gower", na.rm = T, binary = T)
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
    gg_plot <- ggMDSGower_plot()
    if (is.null(gg_plot)) {
      grid::grid.text('Please, first upload a file with data')
    } else {
      gg_plot + ggtitle("PCoA plot")
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
  

  #Help Logic ---------------------------------
  
  
  output$downloadTemplate <- downloadHandler(
    filename <- function() {
      paste("Template_file_dentalAffinities.xlsx")
    },
    
    content <- function(file) {
      file.copy("Template_file_dentalAffinities.xlsx", file)
    },
    contentType = ""
  )
  
  
  #Helper Functions ---------------------------------
  
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
      combinedGroup[i] <- paste0(binary_trait_data[i,2], "+", binary_trait_data[i,3])
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
  

  calculateD2_group1 <- function(binary_trait_data, deltamin= 0.01) {
    # remove columns with wrong data (only NA or 1)
    idx <- c(1:3,which(apply(binary_trait_data[,-(1:3)], 2, function(x) length(unique(na.omit(x)))) > 1) + 3)
    binary_trait_data <- binary_trait_data[,idx]
    # stop
    colnames(binary_trait_data)[1:3] <- c("id", "site", "sex")
    X <- binary_trait_data[,-(1:3)]
    binary_trait_data <- binary_trait_data[!is.na(binary_trait_data$site),]
    tmp <- get_Mn_Mp_group_1(binary_trait_data)
    Sites <- tmp[[1]][1]
    Mn <- tmp[[1]][-1]
    Mp <- tmp[[2]][-1]
    # remove traits with 0 observations
    idx <- which(!apply(Mn == 0, 2, any))
    X <- X[,idx]
    Mn <- Mn[,idx]
    Mp <- Mp[,idx]
    
    n0 = Mn*Mp
    n1 = Mn*(1-Mp)
    # correction for 0
    n0[n1 == 0] = n0[n1 == 0] - 0.5
    n1[n1 == 0] = .5
    n1[n0 == 0] = n1[n0 == 0] - 0.5
    n0[n0 == 0] = .5
    # calculate z
    z <- apply(n1/(n0+n1), 1:2, qnorm)
    rownames(z) = Sites$group1
    
    N.sites = nrow(Sites)
    N.traits = ncol(X)
    n.cases = nrow(X)
    
    # here calculate R
    R = diag(N.traits)
    N.R = diag(N.traits)
    n.unique=N.traits*(N.traits-1)/2
    for(k in 1:N.sites)
    {
      icount = 0
      sto = X[binary_trait_data[,2] == Sites$group1[k],]
      for(i in 1:(N.traits-1)){
        for(j in (i+1):N.traits){
          icount=icount+1
          trait.ij=as.vector(table(sto[,c(j,i)]))
          if(sum(trait.ij==0)>=2 | length(trait.ij)<4){
            r=0
            calc.please=F
          } else{
            calc.please=T
            KDELTA=1
            DELTA=0
            if(trait.ij[1]==0 | trait.ij[4]==0) KDELTA=2
            if(trait.ij[2]==0 | trait.ij[3]==0) KDELTA=KDELTA+2
            
            if(KDELTA==2) DELTA=.5
            if(KDELTA==3) DELTA=-.5
            if(trait.ij[1]==0 & trait.ij[4]==0){
              r=-1
              calc.please=F
            }
            if(trait.ij[2]==0 & trait.ij[3]==0){
              r=1
              calc.please=F
            }
            trait.ij=trait.ij+DELTA*c(1,-1,-1,1)
          }
          
          if(calc.please==T) r = psych::tetrachoric(trait.ij,correct=F)$rho
          
          N.cell = sum(trait.ij)
          
          N.R[i,j] = N.R[i,j] + N.cell
          N.R[j,i] = N.R[j,i] + N.cell
          
          R[i,j] = R[i,j] + r * N.cell
          R[j,i] = R[j,i] + r * N.cell
        }
      }
    }
    
    R = R/N.R
    rownames(R) = colnames(X)
    colnames(R) = colnames(X)
    
    # stop calculations of R
    
    I = diag(N.sites)
    o = rep(1,N.sites)
    J = o %*% t(o)
    w = o/sum(o)
    Delta = (I - o %*% t(w)) %*% z
    Cp = Delta %*% solve(R) %*% t(Delta)
    D2 = (Cp*I) %*% J + J %*% (Cp*I) - 2*Cp
    
    # replace negative values
    D2[D2 <= 0] <- deltamin
    
    rownames(D2) = Sites$group1
    colnames(D2) = Sites$group1
    
    list(MMDMatrix = D2, SDMatrix = NULL, SigMatrix = NULL)
  }
  
  
  calculateD2_group2 <- function(binary_trait_data, deltamin= 0.01) {
    # remove columns with wrong data (only NA or 1)
    idx <- c(1:3,which(apply(binary_trait_data[,-(1:3)], 2, function(x) length(unique(na.omit(x)))) > 1) + 3)
    binary_trait_data <- binary_trait_data[,idx]
    # rename columns
    colnames(binary_trait_data)[1:3] <- c("id", "site", "sex")
    X <- binary_trait_data[,-(1:3)]
    binary_trait_data <- binary_trait_data[!is.na(binary_trait_data$sex),] # filter out rows with NA in sex
    tmp <- get_Mn_Mp_group_2(binary_trait_data)
    Sexes <- tmp[[1]][1]  # Assuming 'site' is replaced with 'sex' in the output
    Mn <- tmp[[1]][-1]
    Mp <- tmp[[2]][-1]
    # remove traits with 0 observations
    idx <- which(!apply(Mn == 0, 2, any))
    X <- X[,idx]
    Mn <- Mn[,idx]
    Mp <- Mp[,idx]
    
    n0 = Mn*Mp
    n1 = Mn*(1-Mp)
    # correction for 0
    n0[n1 == 0] = n0[n1 == 0] - 0.5
    n1[n1 == 0] = .5
    n1[n0 == 0] = n1[n0 == 0] - 0.5
    n0[n0 == 0] = .5
    # calculate z
    z <- apply(n1/(n0+n1), 1:2, qnorm)
    rownames(z) = Sexes$group2
    
    N.sexes = nrow(Sexes)
    N.traits = ncol(X)
    n.cases = nrow(X)
    
    # calculate R
    R = diag(N.traits)
    N.R = diag(N.traits)
    n.unique=N.traits*(N.traits-1)/2
    for(k in 1:N.sexes)
    {
      icount = 0
      sto = X[binary_trait_data[,3] == Sexes$group2[k],]
      for(i in 1:(N.traits-1)){
        for(j in (i+1):N.traits){
          icount=icount+1
          trait.ij=as.vector(table(sto[,c(j,i)]))
          if(sum(trait.ij==0)>=2 | length(trait.ij)<4){
            r=0
            calc.please=F
          } else{
            calc.please=T
            KDELTA=1
            DELTA=0
            if(trait.ij[1]==0 | trait.ij[4]==0) KDELTA=2
            if(trait.ij[2]==0 | trait.ij[3]==0) KDELTA=KDELTA+2
            
            if(KDELTA==2) DELTA=.5
            if(KDELTA==3) DELTA=-.5
            if(trait.ij[1]==0 & trait.ij[4]==0){
              r=-1
              calc.please=F
            }
            if(trait.ij[2]==0 & trait.ij[3]==0){
              r=1
              calc.please=F
            }
            trait.ij=trait.ij+DELTA*c(1,-1,-1,1)
          }
          
          if(calc.please==T) r = psych::tetrachoric(trait.ij,correct=F)$rho
          
          N.cell = sum(trait.ij)
          
          N.R[i,j] = N.R[i,j] + N.cell
          N.R[j,i] = N.R[j,i] + N.cell
          
          R[i,j] = R[i,j] + r * N.cell
          R[j,i] = R[j,i] + r * N.cell
        }
      }
    }
    
    R = R/N.R
    rownames(R) = colnames(X)
    colnames(R) = colnames(X)
    
    # stop calculations of R
    
    I = diag(N.sexes)
    o = rep(1,N.sexes)
    J = o %*% t(o)
    w = o/sum(o)
    Delta = (I - o %*% t(w)) %*% z
    Cp = Delta %*% solve(R) %*% t(Delta)
    D2 = (Cp*I) %*% J + J %*% (Cp*I) - 2*Cp
    
    # replace negative values
    D2[D2 <= 0] <- deltamin
    
    rownames(D2) = Sexes$group2
    colnames(D2) = Sexes$group2
    
    list(MMDMatrix = D2, SDMatrix = NULL, SigMatrix = NULL)
  }
  
  calculateD2_bothgroups <- function(binary_trait_data, deltamin = 0.01) {
    # remove columns with wrong data (only NA or 1)
    idx <- c(1:3, which(apply(binary_trait_data[, -(1:3)], 2, function(x) length(unique(na.omit(x)))) > 1) + 3)
    binary_trait_data <- binary_trait_data[, idx]
    # rename columns
    colnames(binary_trait_data)[1:3] <- c("id", "group1", "group2")
    X <- binary_trait_data[, -(1:3)]
    
    # Combine groups
    tmp <- get_Mn_Mp_bothgroups(binary_trait_data)
    BothGroups <- tmp[[1]][1]  # Assuming 'bothgroups' is the first element
    Mn <- tmp[[1]][-1]
    Mp <- tmp[[2]][-1]
    
    
    
    
    combinedGroup <- rep(NA, nrow(binary_trait_data))
    for (i in 1:nrow(binary_trait_data)){
      combinedGroup[i] <- paste0(binary_trait_data[i,2], "+", binary_trait_data[i,3])
    }
    binary_trait_data$combinedGroup <- combinedGroup
    binary_trait_data <- binary_trait_data[,c(1:3, ncol(binary_trait_data), 4:(ncol(binary_trait_data)-1))]
    colnames(binary_trait_data)[1:4] <- c("id", "group1", "group2", "bothgroups")
    
  
    
    
    # remove traits with 0 observations
    idx <- which(!apply(Mn == 0, 2, any))
    X <- X[, idx]
    Mn <- Mn[, idx]
    Mp <- Mp[, idx]
    
    n0 = Mn * Mp
    n1 = Mn * (1 - Mp)
    # correction for 0
    n0[n1 == 0] = n0[n1 == 0] - 0.5
    n1[n1 == 0] = .5
    n1[n0 == 0] = n1[n0 == 0] - 0.5
    n0[n0 == 0] = .5
    # calculate z
    z <- apply(n1 / (n0 + n1), 1:2, qnorm)
    rownames(z) = BothGroups$bothgroups
    
    N.BothGroups = nrow(BothGroups)
    N.traits = ncol(X)
    n.cases = nrow(X)
    
    # calculate R
    R = diag(N.traits)
    N.R = diag(N.traits)
    n.unique = N.traits * (N.traits - 1) / 2
    for (k in 1:N.BothGroups) {
      icount = 0
      sto = X[binary_trait_data$bothgroups == BothGroups$bothgroups[k], ]
      for (i in 1:(N.traits - 1)) {
        for (j in (i + 1):N.traits) {
          icount = icount + 1
          trait.ij = as.vector(table(sto[, c(j, i)]))
          if (sum(trait.ij == 0) >= 2 | length(trait.ij) < 4) {
            r = 0
            calc.please = F
          } else {
            calc.please = T
            KDELTA = 1
            DELTA = 0
            if (trait.ij[1] == 0 | trait.ij[4] == 0) KDELTA = 2
            if (trait.ij[2] == 0 | trait.ij[3] == 0) KDELTA = KDELTA + 2
            
            if (KDELTA == 2) DELTA = .5
            if (KDELTA == 3) DELTA = -.5
            if (trait.ij[1] == 0 & trait.ij[4] == 0) {
              r = -1
              calc.please = F
            }
            if (trait.ij[2] == 0 & trait.ij[3] == 0) {
              r = 1
              calc.please = F
            }
            trait.ij = trait.ij + DELTA * c(1, -1, -1, 1)
          }
          
          if (calc.please == T) r = psych::tetrachoric(trait.ij, correct = F)$rho
          
          N.cell = sum(trait.ij)
          
          N.R[i, j] = N.R[i, j] + N.cell
          N.R[j, i] = N.R[j, i] + N.cell
          
          R[i, j] = R[i, j] + r * N.cell
          R[j, i] = R[j, i] + r * N.cell
        }
      }
    }
    
    R = R / N.R
    rownames(R) = colnames(X)
    colnames(R) = colnames(X)
    
    # stop calculations of R
    
    I = diag(N.BothGroups)
    o = rep(1, N.BothGroups)
    J = o %*% t(o)
    w = o / sum(o)
    Delta = (I - o %*% t(w)) %*% z
    Cp = Delta %*% solve(R) %*% t(Delta)
    D2 = (Cp * I) %*% J + J %*% (Cp * I) - 2 * Cp
    
    # replace negative values
    D2[D2 <= 0] <- deltamin
    
    rownames(D2) = BothGroups$bothgroups
    colnames(D2) = BothGroups$bothgroups
    
    list(MMDMatrix = D2, SDMatrix = NULL, SigMatrix = NULL)
  }
  
}

shinyApp(ui, server)
