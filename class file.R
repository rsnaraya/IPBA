# # initial code for R
# line2
# line3
# line4 - Shift+Ctrl+C to comment and de-comment of selected lines of code in sequence

# Hit Ctril+Enter to run a particular line
# Zoom in - Shift+Ctrl + + and zoom out Shift+crrl + -

# google for R www.rseek.org

# print function - print anything in the console window
print("Hello world!!")
print("How are you doing today?")
print(2)
print("2","3")
paste("2","3") # string concatenation
a<-paste("2",3,"4")
class(a)
a=10%%4
# :  - sequencing
# $ - indexing (the "elemnent name" operator)
# = - or <- assign (single equal to)
# ~ - model formulae (useful when we need to do modeling ex: y~x)

3 # object widely used
print(3) # not used much. more useful in jupyter since they get over-written

# basic operations
10/2
10+3
10%%0.6 #remainder (%,%)
10%/%0.6 # integer part (%,/,%)

#base mathematical funcitons in R
sum(2,2,3)
sum(1:5)

# mathematical function
# sqrt(x), exp(x), log(x), log(x,n) - log of x to base n
#floor(x) - greatest int <x
# ceiling (x) - smallest int >x

log(100)
log(100,10) # generic log function and we can decide the base
log10(100) # by default base 10

# mean of 1,2,3,4,3,2,1
mean(1,2,3,4,3,2,1) # wrong one - since mean takes only value of first argument. the first argument has to be a vector. hence use "c" - combination
a<-c(1,2,3,4,3,2,1)
mean(a)
# else
mean(c(1:4,3:1))

# assign a value of 5 to p and print p
p<-5
p
# simple artihmetc on p
q<-p+5
q

# create 2 objects to capture day 1
Sales_Shirts <- 60
Sales_Tees<-40

total_sales<-sum(Sales_Shirts,Sales_Tees)
# day 2 dales are 70,20 respectively
Sales_Shirts =70
Sales_Tees=20
# until we override, total_sales doe not change since it is not dynamically overriden. hence we need to do the followng to change the total sales values
total_sales<-sum(Sales_Shirts,Sales_Tees)

# overwtiting can also change the object data type
x<-5
x
class(x)
x<-"jigsaw"
x
class(x)
rm(x)
x
rm(Sales_Shirts,Sales_Tees,total_sales)
sales_shirts
Sales_Tees
total_sales
Sales_Shirts <- 60
Sales_Tees<-40
x<-5
x
class(x)
x<-"jigsaw"
x
class(x)
b=sum(1:1000)
b
c<-c(1:1000)
c
d<-c(1:4.0,3:1.0)
d
e<-c(1,2,3,4,3,2,1)

# to print objects  - not useful
ls()

# Data structures in R.
# Vectors - one-dimesional, one data type
x<-1
x
xvec <- c(1,2,3,4,5)
xvec
class(x)
class(xvec)

# vectors have a constraint of 1 data type - hence numeric becomes chars
xvec1 <- c(1,2,3,4,5,"6")
xvec1
class(xvec1)

xchar<-c("1","2","-5","6")
xchar
# Integer - can use with L
x1<-c(1,2,3,4)
x2<-c(1L,2L,3L,4L)

# Missing values in data
# NA - what does this mean?

# arithmetic operators - no missing values and revenue is ok
price<-c(1,2,3)
units_sold<-c(2,2,1)
revenue<-price*units_sold
sum(revenue)
revenue

# arithmetic operators - missing values and revenue is ok
price<-c(1,2,NA)
units_sold<-c(2,2,1)
revenue<-price*units_sold
sum(revenue)

#Create 3 vectors with 5 elements in each
# 1. Customer name
# 2. Age and 
# 3. wether or not Customer defaulted in paying loan


# 1. Customer name
Customer_Name<-c('Sam','Nick','Chris','Joe','Amenda')
Customer_Name

# 2. Age
Customer_Age<-c(18,25,34,NA,17)
Customer_Age

# 3. wether or not Customer defaulted in paying loan
Customer_Default<-c(TRUE,FALSE,TRUE,FALSE,NA) 
Customer_Default

# find the class
class(Customer_Age)
class(Customer_Name)
class(Customer_Default)

# find the length
length(Customer_Age)
length(Customer_Name)
length(Customer_Default)

# identification of missing values - use is.NA to check if we haev mising values in a vector
is.na(Customer_Age)
is.na(Customer_Default)
is.na(Customer_Name)

# how many missing values in customer age
sum(is.na(Customer_Age))
sum(is.na(Customer_Default))
sum(is.na(Customer_Name))

# explore the which functions to find whcih is the missing value
which(is.na(Customer_Age))
which(is.na(Customer_Default))
which(is.na(Customer_Name))

# Say I have 2 numeric vectors with score of 2 players in 5 ODIs
Player1 <- c(24,51,45,70,19)
Player2 <- c(34,72,11,70,56)

# number of times player1 scored a half-century
Player1>=50
sum(Player1>=50)

# get number of matches where player2 fared better than player 1
sum(Player2>Player1)


# factors - used very much for modeling
# data structure to store categorical variables
gender<-c(1,2,1,2,1,1)
class(gender)
gender<-factor(gender)
class(gender)
# you can use as.factor to change the data types
genderF<-as.factor(gender)

genderF1 <- factor(gender, levels = c(1,2), labels = c("Male","Female"))
genderF <- factor(gender)

class(genderF)
table(genderF)
class(genderF1)
table(genderF1)

print("Rajesh's laptop is working")
print('Modi is from '"BJP"'')

# traversing a vector
brand<-c("Nike","Adidas","Sketchers","Puma","NewBalance")
brand[1] # brand at position 1
brand[4]# brand at position 2
brand[-1] # all brands except first one
brand[-2] # excludes second one
brand[-1:-3] #excludes first to third
length(brand)
brand[2:length(brand)] # from second brand to the end
brand[c(2,4)] # only 2nd and 4th


# matrix
matrix1<-matrix(c(1,2,3,1,5,7),nrow=4,ncol=3)
matrix1
matrix1[,1] # get first column
matrix1[1,] # get all records of first row
matrix1[2,2] # row=2, col=2
matrix1[2] # second element
class(matrix1)

#dataframe - rows and cols- but different data types allowed in each col
# consider the following 4 vectors

product=c("Bag","shoes","belt","belt")

total_price=c(500,1000,150,10000)

color=c("Blue","red","red","blue")

quantity=c(5,2,3,4)

#Creare a dataframe
product_details <- data.frame(product,total_price,color,quantity,
                              stringsAsFactors=FALSE)

product_details[2,3] # second row, 3rd column
product_details[2,] # second row
product_details[,3] # third column
product_details[3] # takes as third column - different from matrix
product_details[,c(2,4)] # all rows but cols 2 and 4. but this is hardcoding

product_details[,c('total_price','quantity')] # all rows but cols 2 and 4 - but without any hardcoding

# one column at a time
product_details['total_price']
product_details$total_price # use $ - for indexed based on total_price

# why we can move to vector from a df
test1<-product_details['total_price']
test2<-product_details$total_price # use $ - for indexed based on total_price
mean(test1) # mean cannot be used on dataframe
mean(test2) # hence covert to vector and use that

# Lists
#Lists : Recursive vectors. 
#Can handle different data types or various sizes
my_list <- list( name = c("Robert", "Emma"), age = c (65, 54,43),
                 retired = c (TRUE, FALSE)) # treated as 3 individual vectors and hence cannot see it from row and column

my_list <- list( name = c("Robert", "Emma"), age = c (65, 54,43),
                 retired = c (TRUE, FALSE),product_details) # including a dataframe

my_list
name1<-my_list$name # treats as char class
name2<-my_list[1] # treats as list
class(name1)
class(name2)

my_list$name[2] # second element of name vector in my_list
my_list['name'][2] # this is wrong and we cannot since this is not a vector. In such cases, we use double-referencing
my_list[['name']][2] # double-referencing on name since the outut is a vector and then we can traverse


# application of lists
age<-c(23,25,26,34,22,14,19,56,76,80,31,22,44,34)
length(age)
bp<-boxplot(age)
bp$out # to get outliers

# importing data
import1<-read.table("D:\\OneDrive - Flutura Business Solutions Pvt Ltd\\Personal\\Analytics course\\IPBA\\R\\Data_R intro and Manipulation\\sample2.csv",sep=",",header = TRUE
) # either two backslash or one forward slash - ex: D:/OneDrive - Flutura Business Solutions PVt Ltd/Personal/Analytics

# header missing
import2<-read.table("D:\\OneDrive - Flutura Business Solutions Pvt Ltd\\Personal\\Analytics course\\IPBA\\R\\Data_R intro and Manipulation\\sample1.txt",sep="\t") 

# missing values not handled
import2<-read.table("D:\\OneDrive - Flutura Business Solutions Pvt Ltd\\Personal\\Analytics course\\IPBA\\R\\Data_R intro and Manipulation\\sample1.txt",sep="\t",header=TRUE) 

# missing values  handled
import2<-read.table("D:\\OneDrive - Flutura Business Solutions Pvt Ltd\\Personal\\Analytics course\\IPBA\\R\\Data_R intro and Manipulation\\sample1.txt",sep="\t",header=TRUE,na.strings=c("Missing","",NA)) 

#convert customer from being a factor to a character
import1$Customer <- as.character(import1$Customer)

default.stringsAsFactors() # by default set to TRUE. So in in cases where we want to take all chars as chars and not string = then make this false (stringsAsFactors = FALSE). Ex: customer last name, first name
# import2<-read.table("D:\\OneDrive - Flutura Business Solutions Pvt Ltd\\Personal\\Analytics course\\IPBA\\R\\Data_R intro and Manipulation\\sample1.txt",sep="\t",header=TRUE,na.strings=c("Missing","",NA), stringsAsFactors = FALSE) 
# else if we accept them as factors, then we have to as.character for each variable


##library(arules)
##install.packages(arules)


#library(lubridate)
#lubridate$lakers
#head(lakers)

################OJ

# set working directory
setwd("D:\\OneDrive - Flutura Business Solutions Pvt Ltd\\Personal\\Analytics course\\IPBA\\R\\Data_R intro and Manipulation")
getwd()
#Reading/ Importing the oj csv file into R
# Create a dataframe oj
oj<-read.csv("oj.csv",header=TRUE)

# view data structure in R
View(oj)

# structure of oj - different data type
str(oj)

# make factor column as factor- since it is more True or False
oj$feat<-as.factor(oj$feat)
str(oj) # now this is a factor variable

# check dimension of rows and cols
dim(oj) # count of row and col
nrow(oj) # row count
ncol(oj) # column count

# first few and last few records
head(oj)
tail(oj)
head(oj,10) # number of records - top 10 - overriding the default for header an tail

# columns names in oj
names(oj)

# getting summary statistics for oj
summary(oj)

# check for missing values
is.na(oj)

# count of missing values
sum(is.na(oj)) # this does not make sense for entire set. we need to do for each variable
colSums(is.na(oj)) # missing values by each column
which(is.na(oj$feat)) # can give which row has a missing value - has to be by col


#######DATA MANIPULATION
# Filtering - selecting specific records
# get the data element in first row and third column
oj[1,3]

# 1,3,6 cols and row  1,2,8,456
oj[c(1,2,8,456),c(1,3,6)]

# first 5 records of brands
oj[c(1:5),"brand"]

# filter out records based on specific conditions

# dplyr instal for data manipulations
install.packages("dplyr") # install on hard drive
library(dplyr) # for the R env to access the dplyr functions. Needs to be run everytime we close the R window

# find if data has tropicana and paperboat brand
oj$brand=="tropicana"
oj$brand=="paperboat"
sum(oj$brand=="tropicana") # get a count if tropicana exists or not in brand
sum(oj$brand=="paperboat")# get a count if paperboat exists or not in brand

# table function - factor/categorical variables - frequecy count
table(oj$brand) # helps the unique values of brands and the count - basically frequency

## filtering and subsetng data based on condition
bs_flt1<-oj[oj$brand=="tropicana",] # selecting all rows where brand is tropicana
class(bs_flt1)

# deplyr code - much more easier to handle multiple conditions
dp_flt1 <- filter(oj,brand=="tropicana") # for oj, pick where brands are tropicana
class (dp_flt1)

dp_flt1 <- filter(oj,brand=="tropicana"|brand=="dominicks") # for oj, pick where brands are tropicana

## selecting or removing columns - thry indexer brakcets
bs_sel1<-oj[,c("week","brand","INCOME")]
head(bs_sel1)

dp_sel1<-select(oj,week,brand,INCOME)
head(dp_sel1)

# deletion of columns
bs_sel2<-oj[,c(-2,-5,-10)]
dp_sel2<-select(oj,-week,-brand,-INCOME) # does not include the 3 columns indicated

# selection _ subseting
# select weeks and stores, when brand is tropicana and no feature advertisement is run
dp_sel3<-filter(oj,brand=="tropicana"& feat==0)
dp_sel3<-select(dp_sel3,week,brand,feat)
# can do in 1 step
dp_sel3<-select(filter(oj,brand=="tropicana"& feat==0),week,brand,feat)


dp_sel3<-oj[oj$brand=="tropicana" & oj$feat==0,c("week","brand","feat")] # through base r

# column calculated as log of income
oj$logInc<-log(oj$INCOME)

# calculate the revenue generated as one more columnd
oj$revenue<-exp(oj$logmove)*oj$price


# in deplyr - use mutate to multiply cols at a time in one single go
dp_oj1<-mutate(oj,dp_logInc=log(INCOME),dp_rev=exp(logmove)*price)
dim(dp_oj1)

# arranging or sorting data
order(oj$week) # gives row indexes in order for week
bs_ord1_asc<-oj[order(oj$week),] # default is ASC
bs_ord1_desc<-oj[order(-oj$week),] # default is DESC

# same through dplyr
dp_ord1_asc<-arrange(oj,week) # asc
dp_ord1_desc<-arrange(oj,-week) # desc
dp_ord1_desc<-arrange(oj,desc(week)) # desc

# grouping or rolling up

# average price for the brands
# base r code is complicated - syntax is higher
# deplyr is better
?aggregate
# aggregate (which variable to summarize, by=list(variable by which a grouping is done), function for aggregation)
aggregate(oj$price,by=list(oj$brand),mean, na.rm=TRUE) # na.rm - ignore those records where there are missing records

# aggregation at brand and feature
aggregate(oj$price,by=list(oj$brand,oj$feat),mean, na.rm=TRUE) # na.rm - ignore those records where there are missing records

tapply(oj$price,oj$brand,mean)
tapply(oj$price,list(oj$brand,oj$feat),mean)

# group by - 2 step process
# group by and then summarize
dp_gr_brand<-group_by(oj,brand) # group by brand and create a new DF
summarize(dp_gr_brand,mean(price),mean(INCOME))
class(dp_gr_brand) # use this only for summarization - cannot do any other manipulation on this
dp_gr_brand<-as.data.frame(dp_gr_brand) # to typecast to a dataframe
class(dp_gr_brand) # now a data frame



       