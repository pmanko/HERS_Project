#Set-up
setwd("C:/Users/lynx/Documents/BIOST515/")
library(uwIntroStats)
library(tidyverse)
library(gridExtra)
hers <- read.csv("HERSdatasub.csv", header=TRUE)
hers$ht <- ifelse(hers$HT=="hormone therapy",1,0)
hers$diab <- ifelse(hers$diabetes=="yes",1,0)
hers$stat <- ifelse(hers$statins=="yes",1,0)
hers$smoke <- ifelse(hers$smoking=="yes",1,0)
hers$exer <- ifelse(hers$exercise=="yes",1,0)
hers$drink <- ifelse(hers$smoking=="yes",1,0)
hers$drink[189] <- NA
hers$drink[203] <- NA
hers$ldldiff <- hers$LDL.1-hers$LDL
hers$ratcho <- hers$HDL/hers$LDL
hers$ratcho.1 <- hers$HDL.1/hers$LDL.1
hers$hdldiff <- hers$HDL.1-hers$HDL

p1 <- ggplot(hers, aes(x=age,y=LDL,pch=as.character(raceth),color=as.character(raceth))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
p2 <- ggplot(hers, aes(x=age,y=LDL,pch=as.character(statins),color=as.character(statins))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
p3 <- ggplot(hers, aes(x=age,y=LDL,pch=as.character(exercise),color=as.character(exercise))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
hersdrink <- hers[-c(189,203),]
p4 <- ggplot(hersdrink, aes(x=glucose,y=LDL,pch=as.character(drinkany),color=as.character(drinkany))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
grid.arrange(p1, p2, p3, p4, nrow=2)

p1 <- hers %>% filter(!is.na(drinkany)) %>% ggplot(aes(x=drinkany, y=HDL)) + geom_boxplot()
p2 <- ggplot(data=hers, aes(x=diabetes, y=HDL)) + geom_boxplot()
p3 <- ggplot(data=hers, aes(x=smoking, y=HDL)) + geom_boxplot()
p4 <- ggplot(data=hers, aes(x=exercise, y=HDL)) + geom_boxplot()
grid.arrange(p1, p2, p3, p4, nrow=2)

p1 <- ggplot(hers, aes(x=age,y=LDL.1,pch=as.character(raceth),color=as.character(raceth))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
p2 <- ggplot(hers, aes(x=age,y=LDL.1,pch=as.character(statins),color=as.character(statins))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
p3 <- ggplot(hers, aes(x=age,y=LDL.1,pch=as.character(exercise),color=as.character(exercise))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
hersdrink <- hers[-c(189,203),]
p4 <- ggplot(hersdrink, aes(x=ht,y=LDL.1,pch=as.character(drinkany),color=as.character(drinkany))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
grid.arrange(p1, p2, p3, p4, nrow=2)



p1 <- ggplot(data=hers, aes(x=HT, y=hdldiff)) + geom_boxplot()
p1
p2 <- ggplot(data=hers, aes(x=HT, y=HDL.1)) + geom_boxplot()
p3 <- ggplot(data=hers, aes(x=HT, y=TG.1)) + geom_boxplot()
grid.arrange(p1,p2,p3,nrow=1)

regress("mean", LDL~age+factor(raceth)+stat+exer+diab+glucose+drink+BMI, data=hers)
summary(lm(hdldiff~ht, data=hers))
