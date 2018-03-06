#Set-up
setwd("C:/Users/DecepticonMeow/Documents/BIOST515/")
library(uwIntroStats)
library(tidyverse)
library(gridExtra)
hers <- read.csv("HERSdatasub.csv", header=TRUE)


#Question 1

#Table Data
descrip(hers)
summary(hers[,3:8])

hersldll <- subset(hers, LDL<100)
descrip(hersldll)
summary(hersldll[,3:8])

hersldlm <- subset(hers, LDL>=100 & LDL<=200)
descrip(hersldlm)
summary(hersldlm[,3:8])

hersldlh <- subset(hers, LDL>200)
descrip(hersldlh)
summary(hersldlh[,3:8])

hershdll <- subset(hers, HDL<50)
descrip(hershdll)
summary(hershdll[,3:8])

hershdlm <- subset(hers, HDL>=50 & HDL<=60)
descrip(hershdlm)
summary(hershdlm[,3:8])

hershdlh <- subset(hers, HDL>60)
descrip(hershdlh)
summary(hershdlh[,3:8])

herstgl <- subset(hers, TG<150)
descrip(herstgl)
summary(herstgl[,3:8])

herstgm <- subset(hers, TG>=150 & TG<=200)
descrip(herstgm)
summary(herstgm[,3:8])

herstgh <- subset(hers, TG>200)
descrip(herstgh)
summary(herstgh[,3:8])

#Checking Associations

hers$smoke <- ifelse(hers$smoking=="yes",1,0)


hers$drink <- ifelse(hers$drinkany=="yes",1,0)
hers$drink[189] <- NA
hers$drink[203] <- NA


hers$fit <- ifelse(hers$exercise=="yes",1,0)


hers$stat <- ifelse(hers$statins=="yes",1,0)


hers$white <- ifelse(hers$raceth=="White",1,0)
hers$black <- ifelse(hers$raceth=="African American",1,0)
hers$other <- ifelse(hers$raceth=="Other",1,0)
hers$diab <- ifelse(hers$diabetes=="yes",1,0)

regress("mean", LDL~age+stat+smoke+drink+fit+white+black+other+SBP+DBP+glucose+
          BMI+diab, data=hers)

regress("mean", LDL~age+stat+smoke+drink+fit+factor(raceth)+SBP+DBP+glucose+
          BMI+diab, data=hers)

regress("mean", HDL~age+stat+smoke+drink+fit+white+black+other+SBP+DBP+glucose+
          BMI+diab, data=hers)

regress("mean", TG~age+stat+smoke+drink+fit+white+black+other+SBP+DBP+glucose+
          BMI+diab, data=hers)

#Plotting Data
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
p4 <- ggplot(hersdrink, aes(x=age,y=LDL,pch=as.character(drinkany),color=as.character(drinkany))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
grid.arrange(p1, p2, p3, p4, nrow=2)

p1 <- ggplot(hers, aes(x=age,y=HDL,pch=as.character(raceth),color=as.character(raceth))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
p2 <- ggplot(hers, aes(x=age,y=HDL,pch=as.character(statins),color=as.character(statins))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
p3 <- ggplot(hers, aes(x=age,y=HDL,pch=as.character(exercise),color=as.character(exercise))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
hersdrink <- hers[-c(189,203),]
p4 <- ggplot(hersdrink, aes(x=age,y=HDL,pch=as.character(drinkany),color=as.character(drinkany))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
grid.arrange(p1, p2, p3, p4, nrow=2)

p1 <- ggplot(hers, aes(x=age,y=TG,pch=as.character(raceth),color=as.character(raceth))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
p2 <- ggplot(hers, aes(x=age,y=TG,pch=as.character(statins),color=as.character(statins))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
p3 <- ggplot(hers, aes(x=age,y=TG,pch=as.character(exercise),color=as.character(exercise))) + geom_point() + 
  geom_smooth(method="loess", se=FALSE, size=1.5) + theme_bw() + xlab("Age (yrs)") +
  ylab("LDL (mg/dl)") + ggtitle("Scatterplot of LDL on Age")
hersdrink <- hers[-c(189,203),]
p4 <- ggplot(hersdrink, aes(x=age,y=TG,pch=as.character(drinkany),color=as.character(drinkany))) + geom_point() + 
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



#Problem 2
hers$ldldiff <- hers$LDL.1-hers$LDL
t.test(hers$ldldiff[hers$HT=="hormone therapy"], hers$ldldiff[hers$HT=="placebo"])

hers$hdldiff <- hers$HDL.1-hers$HDL
t.test(hers$hdldiff[hers$HT=="hormone therapy"], hers$hdldiff[hers$HT=="placebo"])

hers$tgdiff <- hers$TG.1-hers$TG
t.test(hers$tgdiff[hers$HT=="hormone therapy"], hers$tgdiff[hers$HT=="placebo"])
