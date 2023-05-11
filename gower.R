library(vegan)
library(dplyr)

myData <- nina_sites_modified_21feb

#myData <- exampleData

THRESHOLD <- myData[1,]

myData <- myData[-1,]

#Exclude missing values
myData<-myData[which(rowMeans(!is.na(myData)) > 0.7), ]
#myData<-myData[which(colMeans(!is.na(myData)) > 0.7), ]


myData <- myData[!is.na(myData[,2]),]

#binarisation
  for (i in 4:ncol(myData)) {
    myData[,i] <- (myData[,i] >= as.numeric(THRESHOLD[1,i])) + 0
  }


y <- vegdist(myData[,4:ncol(myData)], method="gower", na.rm = T)

pairwise.adonis2(y~GROUP1, data = myData, p.adjust = "holm")

betadisper(y, group = myData$GROUP1, add = T)

permutest(betadisper(y, group = myData$GROUP1, add = T))

TukeyHSD(betadisper(y, group = myData$GROUP1, add = T))

  
  
  #Percnetage of missing values in the data set
  sum(is.na(myData[,4:ncol(myData)]))/
  prod(dim(myData[,4:ncol(myData)]))
  
  
 
  