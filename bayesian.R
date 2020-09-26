install.packages("kernlab")
library(kernlab)
data("spam")

dim(spam)
head(spam[,1:6],3)

#####Visualization
#convert the feature matrix to 0-1
m <- as.data.frame(spam[,-58])
m[m>0]=1

## Remove special characters, nnumbers, capital letter runs etc.

cnames=colnames(m)
m.wn=m[,-c(which(startsWith( cnames, 'num')))]
cmn=colnames(m.wn)
m.wch=m.wn[,-c(which(substr(cmn,1,4)%in% c('char','capi')))]
cch=colnames(m.wch)
dim(m.wch)

#Split the e-mail records into "spam" and "non-spam" category
m_spam=m.wch[which(spam$type=="spam"),]
m_ham=m.wch[which(spam$type=="nonspam"),]

#Count frequencies of features and order them
v_spam <- sort(colSums(m_spam),decreasing=TRUE)
d_spam <- data.frame(word = names(v_spam),freq=v_spam)

#plot wordcloud:

#install.packages("wordcloud")
#install.packages("SnowballC")
#install.packages("RColorBrewer")
library(wordcloud)
library(SnowballC)
library(RColorBrewer)

wordcloud(d_spam$word, freq=d_spam$freq,max.words = 20, random.order = FALSE,scale=c(3,0.2),colors=brewer.pal(8, "Dark2"))

v_ham <- sort(colSums(m_ham),decreasing=TRUE)
d_ham <- data.frame(word = names(v_ham),freq=v_ham)
head(d_ham, 10)

wordcloud(d_ham$word, freq=d_ham$freq,max.words = 20, random.order = FALSE,scale=c(3,0.2),colors=brewer.pal(8, "Dark2"))


#Naive Bayes Classifier
#install.packages("naivebayes")
library(naivebayes)
nbspam=naive_bayes(spam[,-58],spam$type,laplace = 1,usekernel = TRUE)
summary(nbspam)
predspam=(predict(nbspam,newdata=spam[,-58],type="class"))

#Validation with observed:
table(spam$type,predspam)

#Bernoulli Naive Bayes
bnbspam=bernoulli_naive_bayes(m,spam$type,laplace = 0.5)
summary(bnbspam)
predspam=(predict(bnbspam,newdata=as.matrix(m),type="class"))
table(spam$type,predspam)


#Gaussian Naive Bayes
M=as.matrix(spam[,-58])
gnbspam=gaussian_naive_bayes(M,spam$type,laplace = .5)
summary(gnbspam)
predspam=(predict(gnbspam,newdata=M,type="class"))

table(spam$type,predspam)



##########################################
install.packages("R2jags")

library(R2jags)

getwd()
setwd("E:/Teaching/2020/IPBA/June2020/")

reg_dat=read.csv("demand.csv", header = TRUE)
datareg=as.data.frame(reg_dat)
head(datareg)

ln_demand=datareg$ln_demand
ln_price=datareg$ln_price
N=nrow(datareg)

#write model in .txt file

init_values <- function(){
  list(alpha = rnorm(1), beta = runif(1,0,100), sigma = runif(1))
}

params <- c("alpha", "beta", "sigma")
jagsdata <- list("N"=N,"ln_demand"=ln_demand, "ln_price" = ln_price)


regresult <- jags(data = jagsdata, inits = init_values, parameters.to.save = params, model.file = "reg.txt", n.iter = 40000, n.burnin = 5000,n.thin=10)
regresult
traceplot(regresult)






