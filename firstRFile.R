library(timeDate)
library(ggplot2)
library(fields)
library(zoo)
library(RODBC)
library(data.table)
s <- getwd()
##COList <- read.table("c:/doc/levingers/CompletedOrdersList.csv", header = TRUE, sep = ",", quote = "\"",dec=".")
CAList <- read.table("c:/doc/levingers/CustomerAnalysisRStudio.csv", header = TRUE, sep = ",", quote = "\"",dec=".", row.names=NULL)
##head(CAList,10)
##pd<-table(CAList$Customer)
##head(CAList)
## CAListcsv <- read.csv("c:/doc/levingers/CustomerAnalysis.csv", header = TRUE, sep = ",", quote = "\"",dec=".", row.names=NULL)
## theURL <- "http://www.jaredlander.com/data/Tomato%20First.csv"
## tomato<- read.table(file=theURL,header=TRUE, sep=",")
##book4 <- read.table("c:/doc/levingers/Book4.csv", header = TRUE, sep = ",", quote = "\"",dec=".", row.names=NULL)
#This still needs a lot of work my friend.

##CLCIndex <- order(CAList$Customer)
##CAList$Customer[CLCIndex]
##unique(CAList$Customer)
##order(unique(CAList$Customer))
##CAList$Customer[order(CAList$Customer[unique(CAList$Customer)])]
##duplicated(CAList$Customer)as.yearmon(dat$date, "%YM%m")as.yearmon(dat$date, "%YM%m")
##rle(CAList$Customer)
##CAList$Customer
CAList <- sqlQ2
numericDateList <- array(as.numeric(as.POSIXct(CAList$CompletedDate, tz = "", "%d/%m/%Y %H:%M")))
DateCutOff <- as.numeric(as.POSIXct("01/07/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))
DateX <- as.numeric(as.POSIXct("01/07/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))
CustomerNo <- 80
# find all instances of customer = CustomerNo : 
        FilterCustomerIndex <- array(as.numeric(CAList$Customer)==CustomerNo)
# find all instances where visits occurred BEFORE DateCutOff
        FilterDateTimeIndex <- array(numericDateList <= DateCutOff)
FilterIndex <- array(FilterDateTimeIndex*FilterCustomerIndex)
CAListDate <- CAList$CompletedDate[FilterIndex]
FilteredNumericDateList <- numericDateList[rfind(FilterIndex)]
##as.Date(CAListDate[1], "%d/%m/%Y")
##as.POSIXct(CAListDate, tz = "", "%d/%m/%Y %H:%M")

# The following returns testList, a numeric array of dates, in seconds(!) 
# of a customer (defined in CustomerIndex) completed orders:

##FilteredNumericDateList[2:length(FilteredNumericDateList)] - FilteredNumericDateList[1:(length(FilteredNumericDateList)-1)]
##

## This is the mean number of days between visist for a particular customer.
# Let 
p1L<-FilteredNumericDateList[2:length(FilteredNumericDateList)] - FilteredNumericDateList[1:(length(FilteredNumericDateList)-1)]
# Remove zeros:
rfNZ<-rfind(p1L!=0)
p1L[rfNZ]
#Then
meanDAYS <- mean(p1L[(p1L>0)])/(60*60*24)

#Order the data frame
sortedCAList[order(CAList[CAList$CompletedDate]),]

sd<-as.POSIXct(CAList$CompletedDate, tz = "", "%d/%m/%Y %H:%M")
# The following line of code converts sd to an array of date elements 
#where each element only contain a MONTH and a year.
        yMon <- as.yearmon(sd, "%YM%m")
        eomDate<-timeLastDayInMonth(sd)
# This function does the same as the Matlab find() function
        rfind <- function(x)seq(along=x)[as.logical(x)]

DC<-lapply(unlist(X, recursive = FALSE), as.Date)
DCx <- unlist(DC)
DCx <- DCx[(!is.nan(DCx))]
DCx <- DCx[(!is.na(DCx))]
hist(DCx,20)

Amount <- as.numeric(CAList$Value)
yMon
dF <- data.frame(yearMonth = yMon, Amount = Amount)
dFdt<-data.table(dF)
dFdt[,sum(Amount), by=yearMonth]
# Extract the cumulative sum of all completed orders
        cumTurnover <- cumsum(as.numeric(CAList$Value))
        ribbon.plot(sd,cumTurnover,transparent.color="white",...)

#apply(monthList, 1, rfind, na.rm = TRUE)