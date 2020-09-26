install.packages("kernlab")
library(kernlab)
data("spam")

dim(spam)
head(spam)
head(spam[,1:6],3)

# visualization
m <- as.data.frame(spam[,-58])
# create a binary metrics - 0 or 1
m[m>0]=1

cnames = colnames(m)
# drop numbers
m.wn=m[,-c(which(startsWith(cnames,'num')))]
cmn=colnames(m.wn)
# drop special charcters - those that start with char or capi
m.wch = m.wn[,-c(which(substr(cmn,1,4)%in% c('char','capi')))]
cch=colnames(m.wch)
dim(m.wch)

m_spam=m.wch[which(spam$type=="spam"),]
m_ham=m.wch[which(spam$type=="nonspam"),]

# calculate and order the frequency - highest to lowest
v_spam<-sort(colSums(m_spam),decreasing = TRUE)
d_spam<-data.frame(word=names(v_spam),freq=v_spam)

# install libraries
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("RColorBrewer")

# load the libraries
library(wordcloud)
library(SnowballC)
library(RColorBrewer)

wordcloud(d_spam$word,freq=d_spam$freq, max.words=20,random.order=FALSE,scale=c(3,0.2),colors=brewer.pal(8,"Dark2"))

v_ham<-sort(colSums(m_ham),decreasing=TRUE)
d_ham<-data.frame(word=names(v_ham),freq=v_ham)
wordcloud(d_ham$word,freq=d_ham$freq, max.words=20,random.order=FALSE,scale=c(3,0.2),colors=brewer.pal(8,"Dark2"))

# Naives Bayes classifier
install.packages("naivebayes")
library(naivebayes)
# output object of naives_nayes.. Inputs are feature matrix (except 58th col), class variable/matrix, 
nbspam=naive_bayes(spam[,-58],spam$type,laplace=0,usekernel = TRUE)
summary(nbspam)
# predict using output from naives, the entire data as train data, and interested in class
predspam=predict(nbspam,newdata=spam[,-58],type="class")

table(spam$type,predspam)
# too much of false-positive. correct mails going to junk/trash

# Bernoulli Naive Bayes - features are binary variables
bnbspam=bernoulli_naive_bayes(m,spam$type,laplace=0.5)
summary(bnbspam)
predspam=predict(bnbspam,newdata=as.matrix(m),type="class")
table(spam$type,predspam)

#Gaussian Naive Bayes - features are continuous variables
M=as.matrix(spam[,-58])
gnbspam=gaussian_naive_bayes(M,spam$type,laplace=0.5)
summary(gnbspam)
predspam=predict(gnbspam,newdata=M,type="class")
table(spam$type,predspam)


#### MCMC
install.packages("R2jags")
library(R2jags)

### bayesian regression model using Demand data set
setwd("D:\\OneDrive - Flutura Business Solutions Pvt Ltd\\Personal\\Analytics course\\IPBA\\Bayes")
reg_data=read.csv("D://OneDrive - Flutura Business Solutions Pvt Ltd//Personal//Analytics course//IPBA//Bayes//demand.csv",head=T)
head(reg_data)
datareg=as.data.frame(reg_data)

ln_demand = datareg$ln_demand
ln_price = datareg$ln_price
N=nrow(datareg)

# write model in .txt file (reg.txt)
init_values <-function(){
  list(alpha=rnorm(1),beta=runif(1,0,100),sigma=runif(1))
}

params <- c("alpha","beta", "sigma")
jagsdata <- list("N"=N,"ln_demand"=ln_demand,"ln_price"=ln_price)


regresult <- jags(data=jagsdata, inits=init_values,parameters.to.save = params,model.file="reg.txt",n.iter=100000,n.burnin = 5000,n.thin=20)
regresult
traceplot(regresult)

