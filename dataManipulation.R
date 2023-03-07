library(tidyverse)

myData <- nina_sites_modified_21feb

thresholdValues <- myData[1,]

myData <- myData[-1,]

i <- 4

myData1 <- myData %>%
  filter(!is.na(.[[i]])) %>%
  count(GROUP1, .[[i]]) 
 

uniquelist <- myData1$GROUP1 %>%
  unique()


scores <- as.numeric(max(myData1$`.[[i]]`))+1

newData <- as.data.frame(matrix(ncol = length(uniquelist)+2, nrow = scores))
colnames(newData) <- c("Trait", "Score", uniquelist)


for (j in 0:(scores-1)){
 newData$Trait[j+1] <- as.character(colnames(myData)[i])
  newData$Score[j+1] <- j
}




urlfile="https://raw.githubusercontent.com/nmaaranen/Brak/main/brak.csv?token=GHSAT0AAAAAAB6DVDX556NWOEK4UMXJLEMGZAHOCPQ"

raw<-read_csv(url(urlfile))


raw %>% filter(Site=="Brak")->brak

n<-c(9:94)
brak[,n]<-lapply(brak[,n], as.numeric)
f<-c(1:8)
brak[,f]<-lapply(brak[,f], factor)

brak.long<-gather(brak,trait,value,S_UI1:RN_LM1,factor_key = T)

#facet wrap 
brak.long %>%  filter(!is.na(value)) %>%
  ggplot(aes(x=Site2, y=value, shape=Site2, group=Site2)) +
  geom_point()+
  scale_y_continuous(breaks=seq(0,7,1))+
  stat_summary(fun.y=mean, geom="point", shape=7, size=3, color="black") +
  stat_summary(fun.y=mean, geom="point", shape=0, size=3, color="grey") +
  labs(x="Trait frequencies", y="Value") +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(panel.border = element_rect(linetype = "solid", fill = NA), 
        panel.background = element_rect(fill = "white")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(vars(trait))







