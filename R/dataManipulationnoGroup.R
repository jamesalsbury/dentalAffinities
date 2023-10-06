library(tidyverse)

myData <- nina_sites_modified_21feb

thresholdValues <- myData[1,]

myData <- myData[-1,]

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
}








