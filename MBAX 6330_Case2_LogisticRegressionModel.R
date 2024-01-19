rm(list = ls())
setwd("C:/Users/danie/downloads") #change to your working directorybefore running
EC1 <- read.csv("First_data_set.csv", stringsAsFactors = T)
str(EC1)
head(EC1)
summary(EC1)


library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)
library(corrr)
# Function for pseudo R^2
PseudoR2<-function(glmModel){
  #if (length(class(object)) > 1)
  #if (class(glmModel)!="glm" | class(glmModel)!="lm"){
  #stop("Object not of class 'glm'")
  #}
  #else {
  #	glmModel<-glmModel
  #	}
  
  logLikN<-glmModel$null/-2  ##log likelihood, null model
  logLikF<-glmModel$dev/-2  ##log likelihood, full model
  G2<-glmModel$null - glmModel$deviance
  n <- length(glmModel$y)	
  ystar <- predict(glmModel, type="response") 
  class1<-ifelse(ystar >.5,1,0) 
  classtab<-table(class1, glmModel$y, dnn=c("Predicted", "Actual")) ; maxOut<-max(margin.table(classtab, 2))
  p<-glmModel$rank
  penaltyN<- 2*(p)*(p+1)  ; penaltyD<- n-p-1  ; penalty<-penaltyN/penaltyD
  MZystar <- predict(glmModel); sse <- sum((MZystar - mean(MZystar))^2) ; s2 <- switch(glmModel$family$link, "probit" = 1, "logit" = pi^2/3, NA)  #Needed for MZ R2
  Enum<-sum((glmModel$y - ystar)^2); Edenom<-sum((glmModel$y - mean(glmModel$y))^2) #Needed for Effron R2
  
  #R2s
  r2McF<-1-logLikF/logLikN  #Mcfadden's R2
  r2McFA<-1-(logLikF - p-1 )/logLikN #Mcfadden's Adj R2
  r2CS<-1-exp(-G2/n) #ML Cox/Snell R2
  r2N<-(1 - exp((glmModel$dev - glmModel$null)/n))/(1 - exp(-glmModel$null/n))# Nagelkerke/Cragg-Uhler R2
  r2MZ<-sse / (n * s2 + sse)  #McKelvey and Zavoina pseudo R^2, using either the logit or probit link
  r2E<-1-(Enum/Edenom) #Effron R2
  r2C<-(classtab[1] + classtab[4])/n##Count R2 (proportion correctly classified)
  r2CA<-(classtab[1] + classtab[4] - maxOut)/(n - maxOut) ##Adjusted Count R2 (proportion correctly classified)
  aic<-2*(p)+glmModel$dev # AIC
  Caic<-aic + penalty # AIC with a correction for finite sample size; useful with small sample sizes or a lot of predictors
  
  results<-c(McFadden=r2McF, Adj.McFadden=r2McFA, Cox.Snell=r2CS, Nagelkerke=r2N, McKelvey.Zavoina=r2MZ, Effron=r2E, Count=r2C, Adj.Count=r2CA, AIC=aic, Corrected.AIC=Caic)
  return(results)
  
}

# Function for performance metrics
prf <- function(predAct){
  ## predAct is two col dataframe of pred,act
  preds = predAct[,1]
  trues = predAct[,2]
  xTab <- table(preds, trues)
  clss <- as.character(sort(unique(preds)))
  r <- matrix(NA, ncol = 7, nrow = 1, 
              dimnames = list(c(),c('Acc',
                                    paste("P",clss[1],sep='_'), 
                                    paste("R",clss[1],sep='_'), 
                                    paste("F",clss[1],sep='_'), 
                                    paste("P",clss[2],sep='_'), 
                                    paste("R",clss[2],sep='_'), 
                                    paste("F",clss[2],sep='_'))))
  r[1,1] <- sum(xTab[1,1],xTab[2,2])/sum(xTab) # Accuracy
  r[1,2] <- xTab[1,1]/sum(xTab[,1]) # Miss Precision
  r[1,3] <- xTab[1,1]/sum(xTab[1,]) # Miss Recall
  r[1,4] <- (2*r[1,2]*r[1,3])/sum(r[1,2],r[1,3]) # Miss F
  r[1,5] <- xTab[2,2]/sum(xTab[,2]) # Hit Precision
  r[1,6] <- xTab[2,2]/sum(xTab[2,]) # Hit Recall
  r[1,7] <- (2*r[1,5]*r[1,6])/sum(r[1,5],r[1,6]) # Hit F
  r}


# Code to restore zip codes
EC1$zip <- as.character(EC1$zip)
for(i in 1:length(EC1$zip)){
  if(as.numeric(EC1$zip[i]) < 10000){
    EC1$zip[i] <- paste0("0", EC1$zip[i])
  }
}
EC1 <- as.data.frame(unclass(EC1), stringsAsFactors = TRUE)
head(EC1$zip)
str(EC1)

EC2 <- read.csv("Second_data_set.csv", stringsAsFactors = TRUE)
# Code to restore zip codes
EC2$zip <- as.character(EC2$zip)
for(i in 1:length(EC2$zip)){
  if(as.numeric(EC2$zip[i]) < 10000){
    EC2$zip[i] <- paste0("0", EC2$zip[i])
  }
}
EC2 <- as.data.frame(unclass(EC2), stringsAsFactors = TRUE)
head(EC2$zip)
str(EC2)
# Adding NumberHH and MedianIncome
## First read data
us_zip <- read.csv("zip data.csv")
## Code to remove first six characters before zipcode
us_zip$NAME[ us_zip$NAME != 'Geographic Area Name'] <- (gsub("^.{0,6}", "", us_zip$NAME[ us_zip$NAME != 'Geographic Area Name']))
## Remove first row
us_zip = us_zip[-1,]
## Create groups by first four numbers of zip
us_zip$group <- as.factor(str_sub(us_zip$NAME,1,4))
## Convert to numeric and make rest factors
us_zip <- as.data.frame(unclass(us_zip), stringsAsFactors = TRUE)
us_zip <- transform(us_zip, 
                    S1901_C01_001E = as.numeric(S1901_C01_001E), 
                    S1901_C01_012E = as.numeric(S1901_C01_012E))
## Create new data frame for imputed values
us_zip_group <- aggregate(cbind(S1901_C01_001E, S1901_C01_012E) ~ group, data = us_zip, FUN = median)
## Check updates
nrow(us_zip)
head(us_zip$NAME)

## Now merge data by zip code and save as new EC1
EC1 <- left_join(EC1, us_zip[ , c('NAME', 'S1901_C01_001E', 'S1901_C01_012E')], by=c("zip" = "NAME"))
## Change names
colnames(EC1)[27] <- 'NumberHH'
colnames(EC1)[28] <- 'MedianIncome'
## Add group to EC1
EC1$group <- str_sub(EC1$zip,1,4)
## Join with imputed data frame
EC1 <- left_join(EC1, us_zip_group, by='group')
## Impute nulls
EC1$NumberHH <- ifelse(is.na(EC1$NumberHH), EC1$S1901_C01_001E, EC1$NumberHH)
EC1$MedianIncome <- ifelse(is.na(EC1$MedianIncome), EC1$S1901_C01_012E, EC1$MedianIncome)
## Remove group and last two columns
EC1$group <- NULL
EC1$S1901_C01_001E <- NULL
EC1$S1901_C01_012E <- NULL
## Review structure
str(EC1)
## Check nulls in each column 
colSums(is.na(EC1))
## Zip codes with missing values
EC1[rowSums(is.na(EC1)) > 0, 'zip']
## Remove nulls
EC1 <- na.omit(EC1)
nrow(EC1)

## Now merge data by zip code and save as new EC2
EC2 <- left_join(EC2, us_zip[ , c('NAME', 'S1901_C01_001E', 'S1901_C01_012E')], by=c("zip" = "NAME"))
## Change names
colnames(EC2)[24] <- 'NumberHH'
colnames(EC2)[25] <- 'MedianIncome'
## Add group to EC2
EC2$group <- str_sub(EC2$zip,1,4)
## Join with imputed data frame
EC2 <- left_join(EC2, us_zip_group, by='group')
## Impute nulls
EC2$NumberHH <- ifelse(is.na(EC2$NumberHH), EC2$S1901_C01_001E, EC2$NumberHH)
EC2$MedianIncome <- ifelse(is.na(EC2$MedianIncome), EC2$S1901_C01_012E, EC2$MedianIncome)
## Remove group and last two columns
EC2$group <- NULL
EC2$S1901_C01_001E <- NULL
EC2$S1901_C01_012E <- NULL
## Review structure
str(EC2)
## Check nulls in each column 
colSums(is.na(EC2))
## Zip codes with missing values
EC2[rowSums(is.na(EC2)) > 0, 'zip']
## Remove nulls
EC2 <- na.omit(EC2)
nrow(EC2)

# 1: run the script to make verticals into dummy variables

EC1$isFinance <- ifelse(EC1$vertical == "finance", 1, 0)
EC1$isFitness <- ifelse(EC1$vertical == "fitness", 1, 0)
EC1$isHealthC <- ifelse(EC1$vertical == "healthca", 1, 0)
EC1$isHomeImp <- ifelse(EC1$vertical == "homeimp", 1, 0)
EC1$isLegal <- ifelse(EC1$vertical == "legal", 1, 0)
EC1$isOnline <- ifelse(EC1$vertical == "online", 1, 0)
EC1$isRealEst <- ifelse(EC1$vertical == "realesta", 1, 0)
EC1$isSecurity <- ifelse(EC1$vertical == "security", 1, 0)
EC1$isTherapy <- ifelse(EC1$vertical == "therapy", 1, 0)

EC2$isFinance <- ifelse(EC2$vertical == "finance", 1, 0)
EC2$isFitness <- ifelse(EC2$vertical == "fitness", 1, 0)
EC2$isHealthC <- ifelse(EC2$vertical == "healthca", 1, 0)
EC2$isHomeImp <- ifelse(EC2$vertical == "homeimp", 1, 0)
EC2$isLegal <- ifelse(EC2$vertical == "legal", 1, 0)
EC2$isOnline <- ifelse(EC2$vertical == "online", 1, 0)
EC2$isRealEst <- ifelse(EC2$vertical == "realesta", 1, 0)
EC2$isSecurity <- ifelse(EC2$vertical == "security", 1, 0)
EC2$isTherapy <- ifelse(EC2$vertical == "therapy", 1, 0)



#### Creating our Models ####

# base model Danielle Created
glm.fit = glm(Cust_Lobb ~.-vertical - zip - Cust_Psim  , data = EC1, family = binomial) #had to pull out zip because it was running against all zipcodes and freezing R
summary(glm.fit)

### Removing variables by p value one at a time###

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + latepay + Psim_vol + Psim_CC + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 + PsimRev14 + PsimRev15 + PsimRev16 +  Cust_MHW  + Cust_L360 + NumberHH + MedianIncome + isFinance + isFitness + isHealthC + isHomeImp + isLegal + isOnline + isRealEst + isSecurity + isTherapy , data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + latepay + Psim_vol + Psim_CC + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15 + PsimRev16 +  Cust_MHW  + Cust_L360 + NumberHH + MedianIncome + isFinance + isFitness + isHealthC + isHomeImp + isLegal + isOnline + isRealEst + isSecurity + isTherapy , data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + latepay + Psim_vol + Psim_CC + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15 + PsimRev16 +  Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal + isOnline + isRealEst + isSecurity + isTherapy , data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + latepay + Psim_vol + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15 + PsimRev16 +  Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal + isOnline + isRealEst + isSecurity + isTherapy , data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + latepay + Psim_vol + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15 + PsimRev16 +  Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal + isRealEst + isSecurity + isTherapy , data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + latepay + Psim_vol + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15 + PsimRev16 +  Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal + isRealEst + isSecurity, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + latepay + Psim_vol + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15  + Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal + isRealEst + isSecurity, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + latepay + Psim_vol + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15  + Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal + isSecurity, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + Psim_vol + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15  + Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal + isSecurity, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral + Psim_vol + Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15  + Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 + PsimRev08 + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15  + Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 +  PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15  + Cust_MHW  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev07 +  PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev13 +  PsimRev15  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev09 + PsimRev10 + 
                PsimRev11 + PsimRev12 + PsimRev15  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + touches + PsimRev09 +
                PsimRev11 + PsimRev12 + PsimRev15  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + PsimRev09 +
                PsimRev11 + PsimRev12 + PsimRev15  + Cust_L360 + NumberHH + isFinance + isFitness + isHealthC + isHomeImp + isLegal, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + PsimRev09 +
                PsimRev11 + PsimRev12 + PsimRev15  + Cust_L360 + isFinance + isFitness + isHealthC + isHomeImp + isLegal, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + PsimRev09 +
                PsimRev11 + PsimRev12 + PsimRev15  + Cust_L360 + isFinance + isFitness + isHealthC + isHomeImp, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + PsimRev11 + PsimRev12 + PsimRev15  + Cust_L360 + isFinance + isFitness + isHealthC + isHomeImp, data = EC1, family = binomial) 
summary(glm.fit)

glm.fit = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf + PsimRev11 + PsimRev12 + Cust_L360 + isFinance + isFitness + isHealthC + isHomeImp, data = EC1, family = binomial) 
summary(glm.fit)



## Removing Cust_Psim because sd is 0, all of them are psimp customers.
## Uneccessary to view correlation for Cust_MHW and Cust_L360 against Cust_Lobb

unwanted_col <- c("vertical", "zip", "Cust_Psim", "Cust_MHW", "Cust_L360")

EC1_cor <- correlate(EC1[ , !names(EC1) %in% unwanted_col],
                     method = 'pearson',
                     use = 'pairwise.complete.obs')
EC1_cor %>% rplot() # this turns correlation table into a visual

#### Improving our Models ####

# new model Danielle created with unwanted variables removed. Removed features from base model with p_val > .05 and removed features with high correlation 
glm.fit2 = glm(Cust_Lobb ~ org_size + tenure + referral +  Psim_mob + Psim_ACH + Psim_dsf+ Cust_L360 + isFinance + isFitness + isHealthC + isHomeImp, data = EC1, family = binomial) 
summary(glm.fit2)

#removing tenure  and isHomeImp because p_val > .05 in model above
glm.fit2 = glm(Cust_Lobb ~ org_size  + referral + Psim_mob + Psim_ACH  +Psim_dsf  + isFitness +  isFinance + isHealthC + Cust_L360 , data = EC1, family = binomial)
summary(glm.fit2)

#### running pseudoR2 function from headstart script to evaluate model ####

# How to call (use) the two functions above
## Note: you do not need to perform train, test split on EC1 for this function
PseudoR2(glm.fit2)
PseudoR2(glm.fit2)[1] # this will return just the first type of pseudo R^2 metric



#### running test train split on EC1 to evaluate model performance using predAct_df function####

# If you perform a train, test split on EC1, you can use this function
## Note: EC1_test is the name of the subset of EC1 you that set aside for testing, 
## since you know the labels from EC1 you use EC1 for train, test split, not EC2.

set.seed(200)
sample = sample(c(TRUE, FALSE), nrow(EC1), replace = TRUE, prob = c(0.8,0.2))

EC1_train = EC1[sample,]
EC1_test = EC1[!sample,]

glm.fit2 = glm(Cust_Lobb ~ org_size  + referral + Psim_mob + Psim_ACH  +Psim_dsf  + isFitness +  isFinance + isHealthC, data = EC1_train, family = binomial)
summary(glm.fit2)
## Last thing, the predictions passed here need to be binary value (0 or 1), use a threshold of 0.5
## and convert your probability predictions into either class 1 or class 0 for Cust_Lobb
predictions_from_my_model = predict(glm.fit2, newdata = EC1_test, type = "response")
predictions_from_my_model = ifelse(predictions_from_my_model > .5, 1,0)
head(predictions_from_my_model)

predAct_df <- data.frame(predictions_from_my_model, EC1_test$Cust_Lobb)
prf(predAct_df)
# Either of these functions above can help you understand how the performance of your model


#### Code to make our final predictions on EC2 ####
final_predictions = predict(glm.fit2, newdata = EC2, type = "response")





