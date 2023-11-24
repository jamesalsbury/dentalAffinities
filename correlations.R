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

df <- exampleData

if (input$sex_handling == "MALE") {
  df <- df[which(df[,3] == "M"),]
}

df[,3]

AnthropMMD::select_traits(myData, k = 10)


unique(myData[,2])


x <- AnthropMMD::binary_to_table(myData)


deltamin = 0.01
thetadiff <- dentalAffinities::thetadiff_uncorrected
theta <- dentalAffinities::theta_Anscombe
binary_trait_data <- myData
colnames(binary_trait_data)[1:3] <- c("id", "site", "sex")
binary_trait_data_long <- gather(binary_trait_data, trait, value, -(1:3))
gr <- summarise(group_by(binary_trait_data_long, site, trait),
                n = length(na.omit(value)),
                p = ifelse(n>0, mean(value > 0, na.rm=TRUE),0.5))
Mn <- spread(gr[,c("site","trait", "n")], trait, n)
Mp <- spread(gr[,c("site","trait", "p")], trait, p)

M_p <- Mp
M_n <- Mn

# remove traits with n == 0
#This could be user-defined
ind <- which(!apply(M_n == 0, 2, any))
M_p <- M_p[,ind]
M_n <- M_n[,ind]
# end - remove traits with n == 0

VarMatrix <- M_n[1:nrow(M_n), 2:length(M_n[1, ])]
MMDMatrix <- matrix(0, length(M_n[, 1]), length(M_n[, 1]))

for (a in seq_along(VarMatrix[1, ])) {
  for (b in seq_along(MMDMatrix[, 1])) {
    for (c in seq_along(MMDMatrix[1, ])) {
      tmp <- thetadiff(M_n[b,a+1], M_p[b,a+1], M_n[c,a+1], M_p[c,a+1], theta)
      print(tmp)
      tmp <- pmax(tmp, deltamin)
      MMDMatrix[b,c] <- tmp
    }
  }
  
  for (b in seq_along(MMDMatrix[, 1])) {
    for (c in seq_along(MMDMatrix[1, ])) {
      if (b >= c) {
        MMDMatrix[b, c] = 0
      }
    }
  }
  
  VNeg <- 0
  VPos <- 0
  for (b in seq_along(MMDMatrix[, 1])) {
    for (c in seq_along(MMDMatrix[1, ])) {
      if (MMDMatrix[b, c] > 0) {
        VPos = VPos + 1
      }
      if (MMDMatrix[b, c] < 0) {
        VNeg = VNeg + 1
      }
    }
  }
  
  VarMatrix[1, a] = sum(MMDMatrix)
  VarMatrix[2, a] = VPos / (VPos + VNeg)
}

VarStatus <- t(VarMatrix)



MMDMatrix <- matrix(0, length(M_n[, 1]), length(M_n[, 1]))
dimnames(MMDMatrix) <- list(M_n[, 1], M_n[, 1])













