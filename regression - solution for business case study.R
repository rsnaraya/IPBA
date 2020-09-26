## Case study

## Business Problem
## User: sales and marketing manager. Responsible for sales and marketing initiatives for a given region
## Type of business: high-end professional laptop
## Problem Statement: reach out to potential buyer segment of customers
## Assumption for problem statement: high income customers are more likely to purchase a high end laptop as compared to low income customer segment
## Factors influencing income: An individual's annual income results from various factors. Intuitively, it is influenced by the individual's education level, age, gender, occupation, and etc
## Current income classification in the data is >50k and <=50k as two categories of person's income level

## Clean up and read the source data
rm(list=ls())
dat=read.csv("D://Analytics course//IPBA//Regression//business case study assignment//adult-1.csv",head=T)

## have a look at the data
head(dat)
str(dat)
class(dat)
summary(dat)
## which colums have null values and if missing values have to be treated
colSums(is.na(dat))
## indicates no missing values. We are asked not to treat ? in columns - hence no action taken 

## Categorize the income based on the classification of <=50K being 0 (low) and >50K being 1 (high) 
Income_categorized = numeric(nrow(dat))
Income_categorized[dat$income=="<=50K"]=0
Income_categorized[dat$income==">50K"]=1

## read the data and store in intermediate variables
Age = dat$age
ClassOfWork = dat$workclass
FinalWeight = dat$fnlwgt
EducationLevel = dat$education
EducationLevelNumeric = dat$educational.num
MaritalStatus = dat$marital.status
Occupation = dat$occupation
Relationship = dat$relationship
Race = dat$race
Sex = dat$gender
CapitalGains = dat$capital.gain
CapitalLoss = dat$capital.loss
HoursPerWeek = dat$hours.per.week
OriginCountry = dat$native.country
Income = dat$income




####====== Question 1: The null deviance for the fitted model is 
####======  Question 2: The residual deviance for the fitted model is 
# fit a model with 8 predictor variables in total: i.e., all six continuous predictors - age, fnlwgt, education_num, capital_gain, capital_loss, hours_per_week, and two categorical variables - gender and occupation
fit1=glm(Income_categorized~Age+FinalWeight+EducationLevelNumeric+CapitalGains+CapitalLoss+HoursPerWeek+factor(Sex)+factor(Occupation),data=dat,family="binomial")
summary(fit1)
####======  Question 1 - answer: The outcome is Null deviance of 53751 on dof of 48841. So option 2 is selected
####======  Question 2 - answer: The oucome is Residual deviance of 36943 on dof of 38820. So option 3 is selected




####======  Question 3: The value of McFadden's pseudo-R^2 is
# McFadden's R-square calculated based on the following 2 parameters
# find the (maximized) likelihood value from the current fitted model
mod<-glm(Income_categorized~Age+FinalWeight+EducationLevelNumeric+CapitalGains+CapitalLoss+HoursPerWeek+factor(Sex)+factor(Occupation),data=dat,family="binomial")
# find the corresponding value but for the null model - the model with only an intercept and no covariates
nullmod<-glm(Income_categorized~1,data=dat,family="binomial")
# Calculate the Psuedo R-Sq as 1-logLik(mod)/logLik(nullmod)
PseudoRsq = 1-logLik(mod)/logLik(nullmod)
print(PseudoRsq)
####======  Question 3 - answer: 0.31 (rounded up to 2 decimal points) - hence option 1 is chosen





####======  Question 4: The Odds Ratio of Salary with respect to Gender (Males against Females) is
oddsratio<-exp(coef(fit1))
print(oddsratio)
####======  Question 4 - answer: The output, for Odds ration of Salary wrt Male, is 3.26. Hence option 4 is chosen





####======  Question 5: If the confusion matrix is constructed assuming that none of the predicted class labels are conservative (i.e.,y ^_i=1ifp ^_i>0.5), then the proportion of accurate predictions is 
pred2=predict(fit1,type="response") # gives hat(p_i)
pred_prob = pred2
yhat = numeric(length(pred_prob))
yhat[pred_prob>=0.5]=1

# move this to a temp variable
x<-table(Income_categorized, yhat)
AccuratePredictions=(x[1,1]+x[2,2])/(sum(x))
print(AccuratePredictions)
####======  Question 5 - answer: the result is 0.83 - hence option 4 is chosen





####======  Question 6: If you have to choose only one predictor that best explains the income level, which of the 8 predictor variable will it be
fitAge=glm(Income_categorized~Age,data=dat,family="binomial")
fitFinalWeight=glm(Income_categorized~FinalWeight,data=dat,family="binomial")
fitEducationLevelNumeric=glm(Income_categorized~EducationLevelNumeric,data=dat,family="binomial")
fitCapitalGains=glm(Income_categorized~CapitalGains,data=dat,family="binomial")
fitCapilatLoss=glm(Income_categorized~CapitalLoss,data=dat,family="binomial")
fitHoursPerWeek=glm(Income_categorized~HoursPerWeek,data=dat,family="binomial")
fitSex=glm(Income_categorized~factor(Sex),data=dat,family="binomial")
fitOccupation=glm(Income_categorized~factor(Occupation),data=dat,family="binomial")
## Summarize the findings
summary(fitAge) # AIC = 51218
summary(fitFinalWeight) # AIC = 53753 
summary(fitEducationLevelNumeric) # AIC = 47779
summary(fitCapitalGains) # AIC = 48704
summary(fitCapilatLoss) # AIC = 52853
summary(fitHoursPerWeek) # AIC = 51164
summary(fitSex) # AIC = 51270
summary(fitOccupation) # AIC = 47561

# Based on the above, we should pick the lowest AIC to get the best fit model
####======  Question 6 - Answer: Occupation - hence chosen option 3





####======  Question 7 - Out of these 8 variables, which sequence of one variable predictor models that best explains the income level is directionally correct
# Based on Question 6 output, AIC is the basis for determining which is directionally correct
# Lower the AIC, thats the best variable to explain the Income
# hence based on above values, the answer would be as follows
####======  Question 7 - Answer: Occupation (AIC 47561) > Educational num (AIC 47779) > Capital Gain (AIC 48704) > Hours per week (AIC 51164)
# Hence Option 1 is chosen. The rest of the variables are post Hours per week from an AIC number perspective, hence not considered




####======  QUestion 8 - which variables are likely to be collinear, if model had used all 14 variables
# Do a model with all 14 variables
fit2=glm(Income_categorized~Age+FinalWeight+EducationLevelNumeric+CapitalGains+CapitalLoss+HoursPerWeek+factor(Sex)+factor(Occupation)+factor(ClassOfWork)+factor(EducationLevel)+factor(MaritalStatus)+factor(Relationship)+factor(Race)+factor(OriginCountry)+factor(Income),data=dat,family="binomial")
summary(fit2)

# Multicollinearity determination as follows
car::vif(fit2)

# with the above, we get an error "there are aliased coefficients in the model"
#  by defintion alias - refers to the variables that are linearly dependent on others
ld.vars<-attributes(alias(fit2)$Complete)$dimnames[[1]]
# print the linearly dependent variables
print(ld.vars)
# as can be seen in the output, we have 2 variables which are leading to linearly dependent variables which are Class of Work (Without-pay) and Education Level (Some-College)

# now run the fit2 model without these IVs (Education level and Class of Work)
fit2=glm(Income_categorized~Age+FinalWeight+EducationLevelNumeric+CapitalGains+CapitalLoss+HoursPerWeek+factor(Sex)+factor(Occupation)+factor(MaritalStatus)+factor(Relationship)+factor(Race)+factor(OriginCountry)+factor(Income),data=dat,family="binomial")
summary(fit2)
# now run the VIF to see what more variables are collinear
car::vif(fit2)
# we find that MaritalStatus (61.66) and Relationship (76.69) have values >10. Hence these are also collinear
####======  Question 8: hence Option a is chosen - since education and educational.num is collinear (ld.vars)
