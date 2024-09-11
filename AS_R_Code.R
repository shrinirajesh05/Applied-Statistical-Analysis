# 1) Loading the dataset into R and checking for Null Values:
  baseball_hof <- read.csv("baseballHOF.csv",header=TRUE)
  head(baseball_hof)
  sum(is.na(baseball_hof))
  ##----------------------------------------------------------------------------------------------
  
# 2) Categorising the data based on HoF_Status:
  In_HOF <-baseball_hof %>% filter(HoF=="Yes") %>%  select(,-HoF)
  NotIn_HOF <-baseball_hof %>% filter(HoF=="No") %>%  select(,-HoF)
  ##----------------------------------------------------------------------------------------------

# 3) Assumptions of Multivaraiate normality tested on the whole data and on the subgroups of HoF using Mardia Test
library(MVN)
mvn(In_HOF[,2:20],mvnTest="mardia",univariateTest="SW")
mvn(NotIn_HOF[,2:20],mvnTest="mardia",univariateTest="SW")
mvn(baseball_hof[,3:21],mvnTest="mardia",univariateTest="SW")
##----------------------------------------------------------------------------------------------

# 4) Assumption of Homogenity of Covariance MAtrices was tested using Box Test.
library(biotools)
boxM(baseball_hof[,[3:21],baseball_hof[,2])
##----------------------------------------------------------------------------------------------
##Yrs+WAR+WAR7+JAWS+JAWSratio+G+AB+R+H+HR+RBI+SB+BB+BA+OBP+SLG+OPS+OPSadj

# 5) Assumption of independence was tested using Durbin Watson test(autocollinearity).
library(lmtest)
dwtest(backward_LR_model)
##----------------------------------------------------------------------------------------------

# 6) A two sample ttest was performed to all the variables, to identify if there is significant difference in the mean between Yes/No Groups of HoF. 
All variables showed significant differences expect for Jpos. T test performed on Jpos Variable:
  t.test(Jpos ~ HoF, data = baseball_hof,var.equal=TRUE)
##----------------------------------------------------------------------------------------------

# 7) Logistic Regression was performed with 1st level as Not getting inducted and 2nd level as players getting inducted
hof <- baseball_hof %>%
  filter(HoF=="Yes" | HoF=="No") %>%
  mutate(HoF=factor(HoF,levels=c("No","Yes")))
baseball_hof_LR <- glm(HoF~Yrs+WAR+WAR7+JAWS+JAWSratio+G+AB+R+H+HR+RBI+SB+BB+BA+OBP+SLG+OPS+OPSadj,data=hof,family="binomial")
##----------------------------------------------------------------------------------------------

# 8) Performing selection models to proceed with the model with least AIC.
backward_LR_model <- step(baseball_hof_LR, direction="backward",trace=FALSE)
forward_LR_model <- step(baseball_hof_LR, direction="forward",trace=FALSE)
stepwise_LR_model <- step(baseball_hof_LR, direction="both",trace=FALSE)
##----------------------------------------------------------------------------------------------

# 9) The best model with least AIC is backward selection model. This model is further used for classiyfing the training dataset and 
Evaluation of Classification Rule using Substitution Method is as follows
hof <- hof %>%
  mutate(prob=predict(backward_LR_model,type="response")) %>%
  mutate(Classifying_HoF=ifelse(prob>0.50,"Yes","No"))
with(hof,table(HoF,Classifying_HoF))
##----------------------------------------------------------------------------------------------

# 10) Predicting the chances of potential players making the HALL OF FAME using the Logistic Model obtained through the trained dataset of 627 players
baseball_hof_newplayers <- read.csv("baseballHOF2024.csv",header=TRUE)
baseball_hof_newplayers$predictions  <- predict(backward_LR_model, newdata = baseball_hof_newplayers, type = "response")
baseball_hof_newplayers$HoF <- ifelse(baseball_hof_newplayers$predictions  >= 0.5, "Yes", "No")
baseball_hof_newplayers
##----------------------------------------------------------------------------------------------

# 11) Conducting the Likelihood Ratio Test for checking the model fit.
baseball_hof_LRT <- backward_LR_model$null.deviance-baseball_hof_LR$deviance
baseball_hof_LRT
baseball_hof_df <- backward_LR_model$df.null-baseball_hof_LR$df.residual
baseball_hof_df
1-pchisq(baseball_hof_LRT,baseball_hof_df)
##----------------------------------------------------------------------------------------------

# 12) Conducting the Likelihood Ratio Test for checking the model fit.
library(lmtest)
full_model <- glm(HoF ~ Yrs +JAWSratio + G + AB + HR + RBI + BB + OBP, data = hof, family = "binomial")
null_model <- glm(HoF ~ 1, data = hof, family = "binomial")
lr_test <- lrtest(null_model, full_model)
lr_test
##----------------------------------------------------------------------------------------------

# 13) Goodness of fit of the model was performed using Hosmer-Lemeshow Test
library(ResourceSelection)
hof <- hof %>%
  mutate(obs=ifelse(HoF=="Yes",1,0))
hoslem.test(hof$obs,hof$prob)
##----------------------------------------------------------------------------------------------

# 14)Graphical display of the 627 players dataset in R
ggplot(hof,aes(x=1:627,y=prob,color=HoF))+geom_point(size=2)+geom_hline(yintercept=0.5, color = "green",size=1.5)+
  xlab("Players in Data Order")+
  ylab("Probability: In Hall Of Fame")
##----------------------------------------------------------------------------------------------

# 15) Summary Statistics of the dataset consisting of 627 player informations.
summary(baseball_hof)
summary(In_HOF)
summary(NotIn_HOF)

# 16) Summary statistics of the potential candidates segregated into two groups based on the prediction performed using the classification rule.
In_HOF_newplayers <-baseball_hof_newplayers %>% filter(HoF=="Yes")
NotIn_HOF_newplayers <-baseball_hof_newplayers %>% filter(HoF=="No")
summary( In_HOF_newplayers[,2:20])
summary( NotIn_HOF_newplayers[,2:20])

##----------------------------------------------------------------------------------------------
