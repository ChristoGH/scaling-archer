# Lets do the formalities first:

# Load the following Libraries:

library(timeDate)
library(zoo)
library(date)
library(chron)
library(reshape2)    
library(ggplot2)           
library(gridExtra)
# install.packages("devtools")

#library(devtools)
#install_github("plotflow", "trinker")        
#  The start and end dates for SEPTEMBER 2014:
DateZero <- as.numeric(as.POSIXct("01/01/2013 00:00", tz = "", "%d/%m/%Y %H:%M")) 
DateFrom <- as.numeric(as.POSIXct("01/12/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
DateTo <- as.numeric(as.POSIXct("01/01/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))  


#DateFrom <- as.numeric(as.POSIXct("28/11/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
#DateTo <- as.numeric(as.POSIXct("29/11/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        

timeFirstDayInMonth(charvec, format = "%Y-%m-%d", zone = "", 
                    FinCenter = "")
firstDayMonth=function(x)
{       x<-as.Date(x)    
        #x=as.Date(as.character(x), "%d/%m/%Y")
        day <- format(x,format="%d")
        monthYr <- format(x,format="%Y-%m")
        y <- tapply(day,monthYr, min)
        first <- as.Date(paste(row.names(y),y,sep="-"))
        as.factor(first)
}

DailyCustomerAnalysis <- function(Day1, sqlQ3){
#       unique(as.Date(sqlQ3$SalesDate))        
        DateZero <- as.numeric(as.POSIXct("01/01/2013 00:00", tz = "", "%d/%m/%Y %H:%M")) 
        DateFrom <- as.numeric(as.POSIXct(as.Date(Day1), tz = "", "%d/%m/%Y"))        #The unique ServiceItem values are found so:
        DateTo <- as.numeric(as.POSIXct(as.Date(Day1)+1, tz = "", "%d/%m/%Y"))
        
        IndexToFrom <- (sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)
        IndexZeroFrom <- (sqlQ3$SalesDate<DateFrom)&(sqlQ3$SalesDate>=DateZero)
        UniqueSalesOrders <- unique(sqlQ3[IndexToFrom,]$Invoice)
        NumberReturningCustomers <- length(intersect(unique(sqlQ3[IndexToFrom,]$CustomerNo),unique(sqlQ3[IndexZeroFrom,]$CustomerNo)))
        NumberofCustomers <- length(unique(sqlQ3[IndexToFrom,]$CustomerNo))
        NumberNewCustomers <- NumberofCustomers - NumberReturningCustomers
        
        #        DateFrom <- as.numeric(as.Date(Day1), tz = "", "%d/%m/%Y")        #The unique ServiceItem values are found so:
        #        DateTo <- as.numeric(as.Date(Day1)+1, tz = "", "%d/%m/%Y") 
return(c(NumberReturningCustomers, NumberofCustomers, NumberNewCustomers))}

# This invokes the ODBC package to call SQL
library(RODBC)

## This script connects to the latest iLaundry db on this computer
ch <- odbcConnect("SystemHData")

# Remove myTempTable 
Str0 <- 'drop table #myTempTable'
sqlQuery(ch, paste(Str0))
# Done


#This string is to produce a query for SERVICE rendered of ANY type...
        SalesOrdersSQLstring <-'use iLaundry SELECT SALESDOCUMENT.SALESDOCUMENTID as SalesDocumentID, 
        SALESDOCUMENTLINE.SERVICEITEMTYPEID as ServiceType, 
        SERVICETYPE.DESCRIPTION AS ServiceDescription,
        SERVICEITEMTYPE.DESCRIPTION AS ServiceItem,
        SALESDOCUMENT.DOCUMENTNO as Invoice, 
        CUSTOMERRETAIL.CUSTOMERNO AS CustomerNo,
        SALESDOCUMENT.DOCUMENTDATE as SalesDate,  
        CUSTOMERRETAIL.CUSTOMERTITLEID as CustomerID, 
        iif(CUSTOMERRETAIL.CUSTOMERTITLEID =8,\'\',CUSTOMERTITLE.DESCRIPTION)+\' \'+  CUSTOMERRETAIL.FIRSTNAME+\' \'+CUSTOMERRETAIL.SURNAME as Customer,  
        CUSTOMERRETAIL.MOBILENUMBER as Mobile, 
        SALESDOCUMENTLINE.LINETOTAL as Value
        into #myTempTable
        FROM     CUSTOMERRETAIL INNER JOIN
        SALESDOCUMENT ON CUSTOMERRETAIL.CUSTOMERRETAILID = SALESDOCUMENT.CUSTOMERRETAILID INNER JOIN
        CUSTOMERTITLE ON CUSTOMERRETAIL.CUSTOMERTITLEID = CUSTOMERTITLE.CUSTOMERTITLEID INNER JOIN
        SALESDOCUMENTLINE ON SALESDOCUMENT.SALESDOCUMENTID = SALESDOCUMENTLINE.SALESDOCUMENTID INNER JOIN
        SERVICEITEMTYPE ON SALESDOCUMENTLINE.SERVICEITEMTYPEID = SERVICEITEMTYPE.SERVICEITEMTYPEID INNER JOIN
        SERVICETYPE ON SERVICEITEMTYPE.SERVICETYPEID = SERVICETYPE.SERVICETYPEID
        WHERE  (SALESDOCUMENT.DOCUMENTTYPEID = 1) AND  
        (SALESDOCUMENTLINE.SERVICEITEMTYPEID IS NOT NULL) AND 
        (SALESDOCUMENT.DOCUMENTDATE > CONVERT(DATETIME, \'2013-08-01 00:00:00\', 102)) AND 
        (SALESDOCUMENT.DOCUMENTDATE <= CONVERT(DATETIME, \'2015-08-08 00:00:00\', 102))
        GROUP BY SALESDOCUMENT.SALESDOCUMENTID, 
        SALESDOCUMENTLINE.SERVICEITEMTYPEID, 
        SALESDOCUMENT.DOCUMENTNO, 
        SALESDOCUMENT.DOCUMENTDATE, 
        CUSTOMERRETAIL.CUSTOMERTITLEID,
        CUSTOMERTITLE.DESCRIPTION, 
        CUSTOMERRETAIL.FIRSTNAME, 
        CUSTOMERRETAIL.SURNAME, 
        CUSTOMERRETAIL.MOBILENUMBER,
        SALESDOCUMENTLINE.LINETOTAL, 
        SERVICETYPE.DESCRIPTION, 
        CUSTOMERRETAIL.CUSTOMERNO, 
        SERVICEITEMTYPE.DESCRIPTION'

# This line returns no result, Result is read into MyTempTable        
        sqlQ1 <- sqlQuery(ch, paste(SalesOrdersSQLstring))

# This result reads the result from MyTempTable        
        SalesOrdersSQLstring2 <- 'SELECT *, SUM(Value) OVER(partition by ServiceDescription order by  SalesDate)[Cumulative Sum]
        FROM #myTempTable'


SalesOrders<- sqlQuery(ch, paste(SalesOrdersSQLstring2))  

        Str3 <- 'SELECT * FROM #myTempTable'
        sqlQ3<- sqlQuery(ch, paste(Str3))
# Get the sorted unique sales dates extracted from DB
        uniqueDays <- sort(unique(as.Date(sqlQ3$SalesDate)))

IndexToFrom <- (sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)
IndexZeroFrom <- (sqlQ3$SalesDate<DateFrom)&(sqlQ3$SalesDate>=DateZero)

#The following calculates the number of unique sales orders for the month:        
UniqueSalesOrders <- unique(sqlQ3[IndexToFrom,]$Invoice)
NumberReturningCustomers <- length(intersect(unique(sqlQ3[IndexToFrom,]$CustomerNo),unique(sqlQ3[IndexZeroFrom,]$CustomerNo)))
NumberofCustomers <- length(unique(sqlQ3[IndexToFrom,]$CustomerNo))
NumberNewCustomers <- NumberofCustomers - NumberReturningCustomers
ceiling(length(uniqueDays)/7)
dm <- matrix(data = rep(1:ceiling(length(uniqueDays)/7),7), nrow = ceiling(length(uniqueDays)/7), ncol = 7, byrow = FALSE, dimnames = NULL)
dm <- matrix(data = rep(1:7,ceiling(length(uniqueDays)/7)), nrow = ceiling(length(uniqueDays)/7), ncol = 7, byrow = TRUE, dimnames = NULL)
array(data=dm)
# this gives th
weekarray <- rep(1:ceiling(length(uniqueDays)/7),each=7)

analysisArray <- lapply(uniqueDays, DailyCustomerAnalysis, sqlQ3)
dmat <- matrix(data=unlist(analysisArray),ncol=3, byrow=TRUE)
cbound <- cbind(rep(1:ceiling(length(uniqueDays)/7),each=7),uniqueDays, dmat)
cbounddf <- data.frame(cbound)
cbounddf <- setNames(cbounddf, c("Week", "uniqueDays", "ReturningCustomers", "TotalCustomers", "NewCustomers"))
ReturningCustomersCF <- aggregate(ReturningCustomers ~ Week, FUN = sum, data=cbounddf) 
NewCustomersCF <- aggregate(NewCustomers ~ Week, FUN = sum, data=cbounddf) 
AllCustomersCF <- aggregate(TotalCustomers ~ Week, FUN = sum, data=cbounddf) 

#tfD<-timeFirstDayInMonth(as.Date(cbounddf$uniqueDays), format = "%Y-%m-%d", zone = "", FinCenter = "")
#cbounddf$firstMonth<-tfD
cbounddf$Month<-as.numeric(timeFirstDayInMonth(as.Date(cbounddf$uniqueDays), format = "%Y-%m-%d", zone = "", FinCenter = ""))
AllMonthCustomersCF <- aggregate(TotalCustomers ~ Month, FUN = sum, data=cbounddf) 
NewMonthCustomersCF <- aggregate(NewCustomers ~ Month, FUN = sum, data=cbounddf) 
ReturningCustomersCF <- aggregate(ReturningCustomers ~ Month, FUN = sum, data=cbounddf) 

# Now lets graph the three variables:
        xAll <- ggplot(data = AllMonthCustomersCF, aes(x = Month, y = TotalCustomers)) +
                geom_bar(colour="black", fill="#DD8888", stat="identity")
        xAll + ggtitle("Levingers Victory Park - AllCustomers by MONTH")
        
        xNew <- ggplot(data = NewMonthCustomersCF, aes(x = Month, y = NewCustomers)) +
                geom_bar(colour="black", fill="#DD8888", stat="identity")
        xNew + ggtitle("Levingers Victory Park - New Customers by MONTH")
        
        xOld <- ggplot(data = ReturningCustomersCF, aes(x = Month, y = ReturningCustomers)) +
                geom_bar(colour="black", fill="#DD8888", stat="identity")
        xOld + ggtitle("Levingers Victory Park - Returning Customers by MONTH")


        x1 <- ggplot(data = ReturningCustomersCF, aes(x = Week, y = ReturningCustomers)) +
                geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity")
        x1 + ggtitle("Levingers Victory Park - ReturningCustomers")
        
        x2 <- ggplot(data = NewCustomersCF, aes(x = Week, y = NewCustomers)) +
                geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity")
        x2 + ggtitle("Levingers Victory Park - NewCustomers")
        
        x3 <- ggplot(data = AllCustomersCF, aes(x = Week, y = TotalCustomers)) +
                geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity")
        x3 + ggtitle("Levingers Victory Park - TotalCustomers")

# Here endeth the lesson thus far....

aggSDTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceDescription, FUN = sum, data=cbounddf)        
aggSDTableTF <- aggSDTableTF[order(-aggSDTableTF[,2]),]
write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/ServiceTableSeptember2014.csv", x=aggSDTableTF)

#The following extracts the make up of Cobbling broken down by Service Item:       

aggCobblingSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem, FUN = "sum", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Cobbling'),])        
aggAlterationsSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem, FUN = "sum", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Alterations'),])        
aggDryCleaningSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem, FUN = "sum", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Dry-cleaning'),])        
aggCustomerSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ Customer, FUN = "length", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom),])        

sqlQ3$wday <- as.POSIXlt(sqlQ3$SalesDate)$wday
aggWeekDaySTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ wday, FUN = "sum", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom),])        

sqlQ3$mday <- as.POSIXlt(sqlQ3$SalesDate)$mday
mday <- sqlQ3$mday[(sqlQ3$SalesDate>=DateFrom)]

dm <- matrix(data = rep(1:7,6), nrow = 6, ncol = 7, byrow = TRUE, dimnames = NULL)
array(data=dm)

ThisMonth<-sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom),]
ThisMonthCobbling<-sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Cobbling'),]

uniqueCustomers <- length(unique(ThisMonth$CustomerNo))
uniqueCobblingCustomers <- length(unique(ThisMonthCobbling$CustomerNo))
uniqueReturningCustomers <- length(intersect(sqlQ3[!((sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)),]$CustomerNo,unique(ThisMonth$CustomerNo)))
uniqueReturningCobblingCustomers <- length(intersect(sqlQ3[!((sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Cobbling')),]$CustomerNo,unique(ThisMonthCobbling$CustomerNo)))
uniqueNewCustomers <- uniqueCustomers - uniqueReturningCustomers
uniqueNewCobblingCustomers <- uniqueCobblingCustomers - uniqueReturningCobblingCustomers

#--------------------------------------------------------------------------------------        
NoDryCleaningOrders <- sum((sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Dry-cleaning'))
NoCobblingOrders <- sum((sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Cobbling'))
NoAlterationOrders <- sum((sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Alterations'))        
#The following replaces a few annoying abbreviations:
aggCobblingSTTableTF$ServiceItem <- gsub("Cob - ", "", aggCobblingSTTableTF$ServiceItem)
aggCobblingSTTableTF$ServiceItem <- gsub("Misc - ", "", aggCobblingSTTableTF$ServiceItem)
aggAlterationsSTTableTF$ServiceItem <- gsub("Alt - ", "", aggAlterationsSTTableTF$ServiceItem)
aggAlterationsSTTableTF$ServiceItem <- gsub("Misc - ", "", aggAlterationsSTTableTF$ServiceItem)
aggDryCleaningSTTableTF$ServiceItem <- gsub("Dry - ", "", aggDryCleaningSTTableTF$ServiceItem)
aggDryCleaningSTTableTF$ServiceItem <- gsub("Misc - ", "", aggDryCleaningSTTableTF$ServiceItem)

#--------------------------------------------------------------------------------------        


#Here we set the names of the columns:        
aggCobblingSTTableTF <- setNames(aggCobblingSTTableTF, c("ServiceItem", "Value"))
aggAlterationsSTTableTF <- setNames(aggAlterationsSTTableTF, c("ServiceItem", "Value"))
aggDryCleaningSTTableTF <- setNames(aggDryCleaningSTTableTF, c("ServiceItem", "Value"))
#Sort on value:
#        A <- aggCobblingSTTableTF
aggCobblingSTTableTF <- aggCobblingSTTableTF[order(-aggCobblingSTTableTF$Value),]
aggAlterationsSTTableTF <- aggAlterationsSTTableTF[order(-aggAlterationsSTTableTF$Value),]
aggDryCleaningSTTableTF <- aggDryCleaningSTTableTF[order(-aggDryCleaningSTTableTF$Value),]
#Column 3 gives the percentages of the total        
aggCobblingSTTableTF[,3] <- aggCobblingSTTableTF[,2]/sum(aggCobblingSTTableTF[,2])
aggAlterationsSTTableTF[,3] <- aggAlterationsSTTableTF[,2]/sum(aggAlterationsSTTableTF[,2])
aggDryCleaningSTTableTF[,3] <- aggDryCleaningSTTableTF[,2]/sum(aggDryCleaningSTTableTF[,2])
#Column 4 gives a cumulative percentage of the total        
aggCobblingSTTableTF[,4] <- cumsum(aggCobblingSTTableTF[,2]/sum(aggCobblingSTTableTF[,2]))
aggAlterationsSTTableTF[,4] <- cumsum(aggAlterationsSTTableTF[,2]/sum(aggAlterationsSTTableTF[,2]))
aggDryCleaningSTTableTF[,4] <- cumsum(aggDryCleaningSTTableTF[,2]/sum(aggDryCleaningSTTableTF[,2]))
#        aggCobblingSTTableTF <- aggregate(as.numeric(data.matrix(ServiceValue)) ~ CobblingItem, FUN = "sum", data=list(CobblingItem = sqlQ3$ServiceItem[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Cobbling')],
aggCobblingSTTableTF <- setNames(aggCobblingSTTableTF, c("ServiceItem", "Value", "Percentage", "CumulativePerc"))
aggCobblingSTTableTF <- transform(aggCobblingSTTableTF, ServiceItem=reorder(ServiceItem, Value) ) 
aggCobblingSTTableTF80 <- aggCobblingSTTableTF[aggCobblingSTTableTF$CumulativePerc<0.80,]
write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/CobblingTableSeptember2014.csv", x=aggCobblingSTTableTF)

aggAlterationsSTTableTF <- setNames(aggAlterationsSTTableTF, c("ServiceItem", "Value", "Percentage", "CumulativePerc"))
aggAlterationsSTTableTF <- transform(aggAlterationsSTTableTF, ServiceItem=reorder(ServiceItem, Value) ) 
aggAlterationsSTTableTF80 <- aggAlterationsSTTableTF[aggAlterationsSTTableTF$CumulativePerc<0.80,]
write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/AlterationsTableSeptember2014.csv", x=aggAlterationsSTTableTF)

aggDryCleaningSTTableTF <- setNames(aggDryCleaningSTTableTF, c("ServiceItem", "Value", "Percentage", "CumulativePerc"))
aggDryCleaningSTTableTF <- transform(aggDryCleaningSTTableTF, ServiceItem=reorder(ServiceItem, Value) ) 
aggDryCleaningSTTableTF80 <- aggDryCleaningSTTableTF[aggDryCleaningSTTableTF$CumulativePerc<0.80,]
write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/DryCleaningTableSeptember2014.csv", x=aggDryCleaningSTTableTF)

#ServiceValue = sqlQ3$Value[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Cobbling')])),)
#And sort it:
#        aggCobblingSTTableTF <- aggCobblingSTTableTF[order(-aggCobblingSTTableTF[,2]),]
#c <- qplot(ServiceItem, Value, data = aggCobblingSTTableTF80)
#        aggCobblingSTTableTF$ServiceItem <- factor(aggCobblingSTTableTF$ServiceItem, levels=aggCobblingSTTableTF[order(-aggCobblingSTTableTF$Value), "ServiceItem"])
#        aggCobblingSTTableTF80$ServiceItem <- factor(aggCobblingSTTableTF$ServiceItem, levels=aggCobblingSTTableTF[order(-aggCobblingSTTableTF$Value), "ServiceItem"])
#This creates the plot ----------------------------------------------------------------        
x <- ggplot(data = aggCobblingSTTableTF, aes(x = ServiceItem, y = Value)) +
        geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity") +
        coord_flip()
x + ggtitle("Levingers Victory Park - Cobbling - September 2014")
#--------------------------------------------------------------------------------------        
y <- ggplot(data = aggAlterationsSTTableTF, aes(x = ServiceItem, y = Value)) +
        geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity") +
        coord_flip()
y + ggtitle("Levingers Victory Park - Alterations - September 2014")
#--------------------------------------------------------------------------------------        
z <- ggplot(data = aggDryCleaningSTTableTF, aes(x = ServiceItem, y = Value)) +
        geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity") +
        coord_flip()
z + ggtitle("Levingers Victory Park - DryCleaning - September 2014")
#--------------------------------------------------------------------------------------        
#        grid.arrange(x,y,z,ncol = 1)
# Close connection to the DB like so:     
odbcClose(ch)       

