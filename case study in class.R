## case study

# average sales per retailer has increased from year 1 to year 2
# if so, can you come to the similar conclusion for various categories that are selling
# and is the increase due to increase in demand or increase in avg price

# Year 1 - 01 Jul 2006 to 30 Jun 2007; rest is Year 2

# read the required files
setwd("D:\\OneDrive - Flutura Business Solutions Pvt Ltd\\Personal\\Analytics course\\IPBA\\R\\Data_R intro and Manipulation")

orders<- read.csv("Orders.csv", stringsAsFactors = FALSE, na.strings = "") # na.strings will replace all blanks with NA

orderDetails<- read.csv("Orderdetails.csv", na.strings = "")

categories <- read.csv("categories.csv", na.strings = "")

products <- read.csv("products.csv", na.strings = "")

str(orders)
str(orderDetails)
str(categories)
str(products)

# so no missing values to treat for
colSums(is.na(orders))
colSums(is.na(orderDetails))
colSums(is.na(categories))
colSums(is.na(products))

# issue with date for orders.. also need to calcalate the sales = price*quantity*(1-disc)

## Date manipulatios - library called Lubridate
install.packages("lubridate")
library(lubridate)

orders$orderdate<-ymd_hms(orders$orderdate)
class(orders$orderdate)

# dplyr load
library(dplyr)

# we have to get product and sales data
orderDetails$Sales <- orderDetails$qty*orderDetails$unitprice*(1-orderDetails$discount)

# now create one analytical dataset one by one and have one data set to do the ana;ytics

# first merge and create a new table prodCat. Do an inner join - hence no arguments
prodCat <- merge(x= select(categories, categoryid, categoryname), 
                 y = select(products, productid, categoryid), 
                 by = "categoryid")

# now join with OrderDetails table
orderProdCat <- merge(x=orderDetails,y=prodCat, by ="productid")

# now join with Orders table based on date range
ProdCatSalesByDate <- merge(x= select(orders, orderid, orderdate),
                            y = orderProdCat,
                            by = "orderid")

# kpi - date, sales by day

SalesByDate <- group_by(ProdCatSalesByDate, orderdate)
finalSales <- summarize(SalesByDate, 
                        t_sales = sum(Sales),
                        t_qty= sum(qty), 
                        m_Pr = mean(unitprice))

class(finalSales) # now java oriented - hence need to move to dataframe
# Remeber to change final to data frame to do further manipulations on this data
finalSales <-as.data.frame(finalSales)

#now do Year 1 and Year 2 - filtering
o_sales_Y1 <- finalSales[finalSales$orderdate <= "2007-06-30",]
o_sales_Y2 <- finalSales[finalSales$orderdate  > "2007-06-30",]

# mean of sales
mean(o_sales_Y1$t_sales)
mean(o_sales_Y2$t_sales)

# now doing a hypothesis testing to see if the sales for Y2 has increased - hence do a 2 sample t-test (non-paired)
# null hypothesis - no increase in the sales; increase is due to random chance
# alternate hypothesis - there is an increase



