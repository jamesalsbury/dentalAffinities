library(copula)
library(Hmisc)
library(psych)

myData <- nina_sites_modified_21feb

myData <- myData[-1,]

for (i in 4:44){
  myData[,i] <- as.numeric(unlist(myData[,i]))
}


cor(myData[,19], myData[,21], method="kendall")


y <- copula::corKendall(myData[,4:44])

Hmisc::rcorr(myData[,4:44])

psych::mixedCor(myData[,4:44])

y <- cor(myData[,4:44],  use = "pairwise.complete.obs", method="spearman")

