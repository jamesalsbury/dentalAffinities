library(tidyverse)

myData <- nina_sites_modified_21feb

thresholdValues <- myData[1,]

myData <- myData[-1,]

i <- 5

myData1 <- myData %>%
  filter(!is.na(.[[i]])) %>%
  count(GROUP1, .[[i]]) 

colnames(myData1) <- c("GROUP1", "LC", "n")

myData1$LC <- as.numeric(myData1$LC)
 

uniquelist <- myData$GROUP1 %>%
  unique()

uniquescores <- myData1$LC %>%
  unique() %>%
  sort()

newData <- as.data.frame(matrix(ncol = length(uniquelist)+2, nrow = length(uniquescores)))
colnames(newData) <- c("Trait", "Score", uniquelist)


for (j in 1:length(uniquescores)){
 newData$Trait[j] <- as.character(colnames(myData)[i])
  newData$Score[j] <- uniquescores[j]
}

for (j in 1:length(uniquescores)){
  for (k in 3:length(uniquelist)){
   cname <- colnames(newData)[k]
   ourScore <- uniquescores[j]
   ourN <- myData1 %>%
     filter(GROUP1==cname) %>%
     filter(LC==1) %>%
     pull(3)
   newData[j,k] <- ourN
  }
}




