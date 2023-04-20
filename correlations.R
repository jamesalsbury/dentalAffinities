library(copula)
library(Hmisc)
library(psych)

myData <- nina_sites_modified_21feb

myData <- myData[-1,]


#Go from here

for (i in 4:ncol(myData)){
  myData[,i] <- as.numeric(unlist(myData[,i]))
}

y <- cor(myData[,4:ncol(myData)],  use = "pairwise.complete.obs", method="kendall")


for (i in 4:(ncol(myData)-1)){
  for (k in (i+1):ncol(myData)){
    y[k-3, i-3] <- nrow(na.omit(myData[,c(i, k)]))
    #print(i)
  }
}










