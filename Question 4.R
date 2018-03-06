##### Question 4 

#Converting to true/false variables (this was unnecessary)
HERSdatasub$treatment <- HERSdatasub$HT == "hormone therapy"
HERSdatasub$smoke <- HERSdatasub$smoking == "yes"
HERSdatasub$drink <- HERSdatasub$drinkany == "yes" 
HERSdatasub$exercise2 <- HERSdatasub$exercise == "yes"
HERSdatasub$statins2 <- HERSdatasub$statins == "yes"
HERSdatasub$diabetes2 <- HERSdatasub$diabetes == "yes"

#Effect on LDL, adjusting for baseline LDL (Q2)
regress("mean", LDL.1~factor(treatment)+LDL, data=HERSdatasub)

#Adjusting LDL regression for other factors
regress("mean", LDL.1~factor(treatment)+LDL+factor(raceth), data=HERSdatasub)
regress("mean", LDL.1~factor(treatment)+LDL+factor(smoke), data=HERSdatasub)
regress("mean", LDL.1~factor(treatment)+LDL+factor(statins), data=HERSdatasub)
regress("mean", LDL.1~factor(treatment)+LDL+factor(drinkany), data=HERSdatasub)
regress("mean", LDL.1~factor(treatment)+LDL+factor(raceth)+factor(smoke)
        +factor(statins)+factor(drinkany), data=HERSdatasub)

#Effect on HDL, adjusting for baseline HDL
regress("mean", HDL.1~factor(treatment)+HDL, data=HERSdatasub)

#Adjusting HDL regression for other factors
regress("mean", HDL.1~factor(treatment)+HDL+factor(raceth), data=HERSdatasub)
regress("mean", HDL.1~factor(treatment)+HDL+factor(smoke), data=HERSdatasub)
regress("mean", HDL.1~factor(treatment)+HDL+factor(statins), data=HERSdatasub)
regress("mean", HDL.1~factor(treatment)+HDL+factor(drinkany), data=HERSdatasub)
regress("mean", HDL.1~factor(treatment)+HDL+factor(raceth)+factor(smoke)
        +factor(statins)+factor(drinkany), data=HERSdatasub)

#Effect on TG, adjusting for baseline HDL
regress("mean", TG.1~factor(treatment)+TG, data=HERSdatasub)

#Adjusting HDL regression for other factors
regress("mean", TG.1~factor(treatment)+TG+factor(raceth), data=HERSdatasub)
regress("mean", TG.1~factor(treatment)+TG+factor(smoke), data=HERSdatasub)
regress("mean", TG.1~factor(treatment)+TG+factor(statins), data=HERSdatasub)
regress("mean", TG.1~factor(treatment)+TG+factor(drinkany), data=HERSdatasub)
regress("mean", TG.1~factor(treatment)+TG+factor(raceth)+factor(smoke)
        +factor(statins)+factor(drinkany), data=HERSdatasub)
