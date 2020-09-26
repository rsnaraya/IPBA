data(mtcars)

y = mtcars$am
mpg = mtcars$mpg
cyl = mtcars$cyl
hp = mtcars$hp
wt = mtcars$wt

#=============================
# Basic model fit......
#=============================

fit1 = glm(y~mpg+cyl+hp+wt,family="binomial")
summary(fit1)


res1=residuals(fit1,type="deviance")
res2=residuals(fit1,type="pearson")

pred_prob = predict(fit1,type="response") # gives hat(p_i)
pred1=predict(fit1) # gives log-odds of hat(p_i)


# linearity
plot(mpg,pred1)
plot(hp,pred1)
plot(wt,pred1)


# Diagnostics
barplot(hatvalues(fit1))
car::vif(fit1)


#===============================
# automatic variable selection
#===============================

null = glm(y~1,family="binomial")
full = glm(y~(mpg+cyl+wt+hp),family="binomial")

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = glm(aicmodel$terms,family="binomial")
summary(finalaic)


pred_prob = predict(finalaic,type="response")

#===============================
# Gain-Lift analysis
#===============================

dat1=NULL
library(funModeling)
dat1$y = y
dat1$pred_prob = pred_prob
gain_lift(data=dat1,score="pred_prob",target="y")


# score.point = cutoff for p-hat
# sum(pred_prob>score.point[k]) = Population[k]
# Gain = sensitivity 
# Lift = Gain/Population



