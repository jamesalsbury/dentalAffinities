library(tidyverse)

myData <- nina_sites_modified_21feb

thresholdValues <- myData[1,]

myData <- myData[-1,]

combinedGroup <- rep(NA, nrow(myData))

for (i in 1:nrow(myData)){
  combinedGroup[i] <- paste0(myData[i,]$GROUP1, "+", myData[i,]$GROUP2)
}

myData$combinedGroup <- combinedGroup

myData <- myData[,c(1:3, ncol(myData), 4:(ncol(myData)-1))]

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
}








