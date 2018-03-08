setwd("~/Downloads")
hersdata <- read.csv("HERSdatasub.csv", header=TRUE)
library("uwIntroStats")

#Indicator for tx arms, placebo and all no's are 0's
hersdata$HT <- as.integer(hersdata$HT)
hersdata$HT <- ifelse(hersdata$HT=="2", 0,1)

hersdata$smoking <- as.integer(hersdata$smoking)
hersdata$smoking <- ifelse(hersdata$smoking=="2", 1,0)

hersdata$drinkany <- as.integer(hersdata$drinkany)
hersdata$drinkany <- ifelse(hersdata$drinkany=="2", 1,0)

hersdata$exercise <- as.integer(hersdata$exercise)
hersdata$exercise <- ifelse(hersdata$exercise=="2", 1,0)

hersdata$statins <- as.integer(hersdata$statins)
hersdata$statins <- ifelse(hersdata$statins=="2", 1,0)

#Effect on LDL, adjusting for baseline LDL
ldl <- regress("mean", LDL.1~HT+LDL, data=hersdata)
ldl

#Effect on HDL, adjusting for baseline HDL
hdl <- regress("mean", HDL.1~HT+HDL, data=hersdata)
hdl

#Effect on TG, adjusting for baseline TG
tg <- regress("mean", TG.1~HT+TG, data=hersdata)
tg
