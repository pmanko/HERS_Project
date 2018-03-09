library(uwIntroStats)
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(ggthemes)
library(kableExtra)
library(knitr)

hers <- as.data.table(read.csv("data/HERSdatasub.csv", header=TRUE))

# Variable Setup
hers[,LDL.diff:=LDL.1-LDL]
hers[,HDL.diff:=HDL.1-HDL]
hers[,BMI.diff:=BMI.1-BMI]
hers[,TG.diff:=TG.1-TG]
hers[,glucose.diff:=glucose.1-glucose]
hers[,TG.HDL.ratio:=TG/HDL]
hers[,TG.HDL.ratio.1:=TG.1/HDL.1]
hers[,TG.HDL.ratio.ratio:=TG.HDL.ratio.1/TG.HDL.ratio]

# Indicator variable for predictor
hers[,treatment:=ifelse(HT=='placebo', 0, 1)]

# Other Indicators
hers[,smoking:=ifelse(smoking=='yes', 1, 0)]
hers[,drinkany:=ifelse(drinkany=='yes', 1, 0)]
hers[,exercise:=ifelse(exercise=='yes', 1, 0)]
hers[,statins:=ifelse(statins=='yes', 1, 0)]
hers[,diabetes:=ifelse(diabetes=='yes', 1, 0)]

# African American = 0, other = 1, White = 2
hers[,raceth:=factor(raceth)]

bp_level_names = c('normal', 'elevated', 'stage1', 'stage2')

bp_cat <- function(sbp, dbp) {
  if(sbp < 120 && dbp < 80) 0
  else if(sbp < 130 && dbp < 80) 1
  else if(sbp < 140 || dbp < 90) 2
  else if((sbp >= 140 && sbp < 180) || (dbp >= 90 && dbp < 120)) 3
  else if(sbp > 180 || dbp > 120) 4
  else NA
}

hers[,bp.category:=bp_cat(SBP, DBP),by=1:nrow(hers)]
hers[,bp.category:=factor(bp.category, labels=bp_level_names)]
table(hers$bp.category)

outcome_vars <- c("LDL.diff", "HDL.diff", "TG.diff")



do.call("regress", list("mean", as.formula(f), data=as.name("hers")))


# race, statins, drinking, smoking

# LDL.diff effect modification

risk_factors <- c("smoking", "drinkany", "raceth", "statins", 'diabetes', 'glucose')
clean_hers <- hers[complete.cases(hers[,..risk_factors])]

# LDL.diff

## Full adjusted model from q2
m <- lm(LDL.diff ~ treatment + age + raceth + BMI + smoking + drinkany + exercise + statins + bp.category + diabetes + glucose, data=hers)

## Anova
lm_a0 <- lm(LDL.diff ~ treatment, data=clean_hers)
lm_a1 <- lm(LDL.diff ~ treatment + raceth + smoking + drinkany, data=clean_hers)
lm_a2 <- lm(LDL.diff ~ treatment + raceth + smoking + drinkany + statins, data=clean_hers)
anova(lm_a0, lm_a1, lm_a2)

## Basically, statins make model significantly different - 

lm0 <- lm(LDL.diff ~ treatment, data=hers)
lm1 <- lm(LDL.diff ~ treatment + statins, data=hers)

## EFFECT MODIFICATION: Interaction term is significant - report this finding, and the differnet slopes for statins/ w/o statins
lm2 <- lm(LDL.diff ~ treatment * statins, data=hers)



## CONFOUNDING: associated with predictor, causual w/ outcome, different pathway; unadjusted and adjusted are different
## PRECISION: Associated with outcome, not predictor; check residual standard error, SE for predictor is lower

### Outcome Association:
regress("mean", LDL.diff ~ statins, data=hers)  
regress("mean", LDL.diff ~ smoking, data=hers)  
regress("mean", LDL.diff ~ drinkany, data=hers)  
summary(lm(LDL.diff ~ raceth, data=hers))
### Statins: yes (p < 0.00005)
### Others: no p = (.762, .0791, .995/.812)

### ! only statins are possible precision/confounding


### POI Association:
regress("mean", treatment ~ statins, data=hers)  
regress("mean", treatment ~ smoking, data=hers)  
regress("mean", treatment ~ drinkany, data=hers)  
summary(lm(treatment ~ raceth, data=hers))

### None are siginifcantly associated

######################################################################

# HDL.diff (race)

## Full adjusted model from q2
m <- lm(HDL.diff ~ treatment + age + raceth + BMI + smoking + drinkany + exercise + statins + bp.category + diabetes + glucose, data=hers)
anova(m)


## Anova
lm_a0 <- lm(HDL.diff ~ treatment, data=clean_hers)
lm_a1 <- lm(HDL.diff ~ treatment + statins + smoking + drinkany, data=clean_hers)
lm_a2 <- lm(HDL.diff ~ treatment + raceth + smoking + drinkany + statins, data=clean_hers)
anova(lm_a2)
## Only race significantly associated

anova(lm_a0, lm_a1, lm_a2)
## Again, adding race to the model makes it explains more of the variance

## Basically, race make model significantly different - 
lm0 <- lm(HDL.diff ~ treatment, data=hers)
lm1 <- lm(HDL.diff ~ treatment + raceth, data=hers)
## the Race-other cat variable is sig


## EFFECT MODIFICATION: Interaction term is significant - report this finding, and the differnet slopes for statins/ w/o statins
lm2 <- lm(HDL.diff ~ treatment * raceth, data=hers)
summary(lm2)
## raceOther is an effect modifier - p value = 0.0483 - slightly statistically signifcant at alpha = .05

## CONFOUNDING: associated with predictor, causual w/ outcome, different pathway; unadjusted and adjusted are different
## PRECISION: Associated with outcome, not predictor; check residual standard error, SE for predictor is lower

### Outcome Association:
regress("mean", HDL.diff ~ statins, data=hers)  
regress("mean", HDL.diff ~ smoking, data=hers)  
regress("mean", HDL.diff ~ drinkany, data=hers)  
summary(lm(HDL.diff ~ raceth, data=hers))
### All but raceOther: no p = (0.799, 0.921, 0.3231, raceWhite=0.140)
### ! only raceOther is possible precision/confounding, but none are sign. associated w/ treatement - makes sense for a clinical trial

### POI Association:
regress("mean", treatment ~ statins, data=hers)  
regress("mean", treatment ~ smoking, data=hers)  
regress("mean", treatment ~ drinkany, data=hers)  
summary(lm(treatment ~ raceth, data=hers))

### None are siginifcantly associated


######################################################################################
# TG.diff (race, daibetes, glucose)

## Full adjusted model from q2
m <- lm(TG.diff ~ treatment + age + raceth + BMI + smoking + drinkany + exercise + statins + bp.category + diabetes + glucose, data=hers)
anova(m)
## Race = 0.00441
## Diabetes = < 0.00005
## Glucose = 0.00205

## Anova
lm_a0 <- lm(TG.diff ~ treatment, data=clean_hers)
lm_a1 <- lm(TG.diff ~ treatment + statins + smoking + drinkany, data=clean_hers)
lm_a2 <- lm(TG.diff ~ treatment + raceth + smoking + drinkany + statins, data=clean_hers)
lm_a3 <- lm(TG.diff ~ treatment + raceth + smoking + drinkany + statins + diabetes + glucose, data=clean_hers)
anova(lm_a2)
## Only race significantly associated

anova(lm_a0, lm_a1, lm_a2, lm_a3)
## Again, adding race to the model makes it explains more of the variance (decreases residual sum of squares (RSS in the table))

## Basically, race make model significantly different - 
lm0 <- lm(TG.diff ~ treatment, data=hers)
lm1 <- lm(TG.diff ~ treatment + raceth + diabetes + glucose, data=hers)
summary(lm1)
## the raceOther, raceWhite, diabetes, and glucose slopes are sig different than 0

## EFFECT MODIFICATION: Interaction term is significant - report this finding, and the differnet slopes for statins/ w/o statins
lm_i0 <- lm(TG.diff ~ treatment * raceth, data=hers)
summary(lm_i0)
## no interaction at alpha=0.05; raceWhite*treatment possible interaction term (p=0.777) - not effect mod

lm_i1 <- lm(TG.diff ~ treatment * diabetes, data=hers)
summary(lm_i1)
## Diabetes interaction: Definitely effect mod. (p=0.00082)

lm_i2 <- lm(TG.diff ~ treatment * glucose, data=hers)
summary(lm_i2)
## no interaction at alpha=0.05


## CONFOUNDING: associated with predictor, causual w/ outcome, different pathway; unadjusted and adjusted are different
## PRECISION: Associated with outcome, not predictor; check residual standard error, SE for predictor is lower

### Outcome Association:
regress("mean", TG.diff ~ statins, data=hers)  
regress("mean", TG.diff ~ smoking, data=hers)  
regress("mean", TG.diff ~ drinkany, data=hers)  
regress("mean", TG.diff ~ diabetes, data=hers)  
regress("mean", TG.diff ~ glucose, data=hers)  
summary(lm(TG.diff ~ raceth, data=hers))
### All but raceOther, glucose: no p = (0.274, 0.203, 0.431, glucose=0.188)
### !  raceOther (p = 0.0012) 
### !  raceWhite (p = 0.0316)
### !  diabetes (p<0.00005)

### POI Association:
regress("mean", treatment ~ statins, data=hers)  
regress("mean", treatment ~ smoking, data=hers)  
regress("mean", treatment ~ drinkany, data=hers)  
regress("mean", treatment ~ diabetes, data=hers)  
regress("mean", treatment ~ glucose, data=hers)  
summary(lm(treatment ~ raceth, data=hers))

### None are siginifcantly associated with the Predicotr of Interest (treatment) - Again, this makes a lot of sense. 
### The point of a randomized clinical trial is to make the predictor of interest the only significantly different
### variable between treatment groups; all other possible covariates should be taken care of by randomization.

#######################################################################################

