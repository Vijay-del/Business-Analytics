# Part A
# Importing the data set
library(readr)
retail <- read_csv("C:/Users/Vijay/Downloads/Online_Retail(2).csv")
View(retail)

#Q 1a:
pnorm(700,494,100,lower.tail = FALSE) # desired score will be on right side to the mean

#Q 1b:
pnorm(450,494,100,lower.tail = TRUE)-pnorm(350,494,100,lower.tail = TRUE)

#Q 2:
z<-qnorm(0.8665)
avg<- 449-(z*36)
avg

#Q 3:
kent<-c(59,68,78,60)
los<-c(90,82,78,75)
NUM<-sum((kent-mean(kent))*(los-mean(los)))
DEN<-(sqrt(sum((kent-mean(kent))^2))*sqrt(sum((los-mean(los))^2)))
NUM/DEN

#Q 4:
table(retail$Country)
con<-table(retail$Country)/NROW(retail$Country)*100
con
con[con>1]

#Q 5:
retail$TransactionValue<-retail$Quantity*retail$UnitPrice

#Q 6:
sum.con<-tapply(retail$TransactionValue,retail$Country,sum)
sum.con
sum.con[sum.con>130000]

###########GOLDEN QUESTION####################
Temp=strptime(retail$InvoiceDate,format='%m/%d/%Y %H:%M',tz='GMT')
retail$New_Invoice_Date <- as.Date(Temp)
retail$New_Invoice_Date[20000]- retail$New_Invoice_Date[10]
retail$Invoice_Day_Week= weekdays(retail$New_Invoice_Date)
retail$New_Invoice_Hour = as.numeric(format(Temp, "%H"))
retail$New_Invoice_Month = as.numeric(format(Temp, "%m"))

#Q 7a:
tapply(retail$TransactionValue,retail$Invoice_Day_Week,NROW)
y<-tapply(retail$TransactionValue,retail$Invoice_Day_Week,NROW)/NROW(retail$TransactionValue)*100
y[c(3,2,5,6,4,1)]

#Q 7b:
tapply(retail$TransactionValue,retail$Invoice_Day_Week,sum)
z<-tapply(retail$TransactionValue,retail$Invoice_Day_Week,sum)/sum(retail$TransactionValue)*100
z[c(3,2,5,6,4,1)]

#Q 7c:
tapply(retail$TransactionValue,retail$New_Invoice_Month,sum)
x<-tapply(retail$TransactionValue,retail$New_Invoice_Month,sum)/sum(retail$TransactionValue)*100
x
names(x)<-c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

#Q 7d:
retail$InvoiceDate[max(retail$TransactionValue[retail$Country=="Australia"])]

#Q 7e:
s1<-table(retail$New_Invoice_Hour)
abs(diff(s1))
which.min(abs(diff(s1)))
abs(diff(s1))[1] #between 7AM-8AM is the minimum number of customers will use the website.

#Q 8:
plot1<-hist(retail$TransactionValue[retail$Country=="Germany"],breaks = 10,xlab="transactions made from Germany",ylab="No.of transactions",ylim = c(0,10000),xlim = c(-500,1000),col = "Red")
plot1
text(plot1$mids,plot1$counts,labels = plot1$counts, adj=c(0.5,-0.5))

#Q 9:
sort(tapply(retail$TransactionValue,retail$CustomerID,length),decreasing = TRUE)[1] #Customer with highest number of transcations 
sort(tapply(retail$TransactionValue,retail$CustomerID,sum),decreasing = TRUE)[1] #Most valuable Customer

#Q 10:
colMeans(is.na(retail))*100

#Q 11:
fun1<-function(x){
  count<-sum(is.na(x))
  return(count)
}
tapply(retail$CustomerID,retail$Country,fun1)

#Q 12:(Golden Question)
zz<-as.data.frame(table(retail$CustomerID))
names(zz)<-c("Cus.ID","No.of Visits")
round(mean(abs(diff(zz$`No.of Visits`)))) #Average number of days between consecutive shopping by a customer

#Q 13:
NROW(retail$Quantity[retail$Quantity<0 & retail$Country=="France"])/NROW(retail)*100

#Q 14:
sort(tapply(retail$TransactionValue,retail$Description,sum),decreasing = TRUE)[1]

#Q 15:
length(unique(retail$CustomerID))




















