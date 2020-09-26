rm(list=ls())
dat=read.csv("D://OneDrive - Flutura Business Solutions Pvt Ltd//Personal//Analytics course//IPBA//Regression//Class problems//50_Startups.csv",head=T)

Size = numeric(nrow(dat))
Size[dat$Profit>=100000]=0
Size[dat$Profit<100000]=1


Admin = dat$Administration
Mark = dat$Marketing.Spend
states = numeric(nrow(dat))
states[dat$State=="California"]=1

#States = as.double(dat$State)

fit1=glm(Size~Admin+Mark+factor(states),data=dat,family="binomial")
#fit1=glm(Size~Admin+Mark+ordered(states),data=dat,family="binomial")
summary(fit1)
fit2=glm(Size~Admin+Mark,data=dat,family="binomial")
summary(fit2)









#============================
# Residual analysis
#============================

pred1=predict(fit1) # gives log-odds of hat(p_i)
pred2=predict(fit1,type="response") # gives hat(p_i)
##predict(fit1,interval="confidence")

res1=residuals(fit1,type="deviance")
res2=residuals(fit1,type="pearson")

# Multicollinearity
car::vif(fit1)


# diagnostics
# x- outliers
barplot(hatvalues(fit1))

# linearity
plot(Admin,pred1)
plot(Mark,pred1)


# concordance & discordance
cor(Size,pred2,method="kendall")

#=============================
### Linearity of IVs
#============================

fit1=glm(Size~Admin+Mark+Admin*Mark+states,family="binomial")

# automatic variable selection
null = glm(Size~1)
full = fit1

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = glm(aicmodel$terms,family="binomial")
summary(finalaic)


#=============================
### Confusion matrix
#============================

pred_prob = pred2
yhat = numeric(length(pred_prob))
#yhat[pred_prob>=0.2935]=1
yhat[pred_prob>=0.75]=1


table(Size, yhat)


library(funModeling)
dat$Size = Size
dat$pred_prob = pred_prob
ff=gain_lift(data=dat,score="pred_prob",target="Size")

#==================================
# ROC Curve
library(pROC)
rr= ROC(Size,pred_prob)
plot(rr)
auc(rr)

#==================================
### Hosmer-Lemeshow Goodness of Fit
### How well our model fits depends on the difference between the model and the observed data.  One approach for binary data is to implement a Hosmer Lemeshow goodness of fit test.
#==================================

install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(dat$Size, fitted(fit1))
hoslem.test(dat$Size, fitted(fit2))
##Our model appears to fit well because we have no significant difference between the model and the observed data (i.e. the p-value is above 0.05)
