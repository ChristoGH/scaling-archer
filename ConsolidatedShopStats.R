

wd <- getwd()
setwd("C:/rProgramming/ShopStats/Graphs")

# Lets get the data here:
chShop <- odbcConnect("shopData")
        salesFrame <- sqlFetch(chShop, "salesFrame")
        salesFrame$Value<-as.numeric(data.matrix(salesFrame$Value))
                salesFrame$ServiceItem <- gsub("Cob - ", "", salesFrame$ServiceItem)
                salesFrame$ServiceItem <- gsub("Misc - ", "", salesFrame$ServiceItem)
                salesFrame$ServiceItem <- gsub(" - Each", "", salesFrame$ServiceItem)
                salesFrame$ServiceItem <- gsub(" 1/2", "", salesFrame$ServiceItem)

        productFrame <- sqlFetch(chShop, "productFrame")
        productFrame$Value<-as.numeric(data.matrix(productFrame$Value))
        #The following adds a column to the data which is a string identifying the month and year of each entry.
        #This is for uniquely identifying items BY MONTH
                salesFrame$yMon <- as.yearmon(as.Date(salesFrame$SalesDate), "%YM%m")
odbcClose(chShop) 

# Lets set the sales dates to something we UNDERSTAND:
        salesFrame$SalesDate<-as.POSIXct(as.Date(salesFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
        productFrame$SalesDate<-as.POSIXct(as.Date(productFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")

#  The start and end dates for THE MONTH under consideration:
        #DateZero defines the start of time
                DateZero <- as.numeric(as.POSIXct("01/01/2013 00:00", tz = "", "%d/%m/%Y %H:%M")) 

        #DateFrom defines the START of the period/month under investigation:
                DateFrom <- as.numeric(as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))

        #DateTo defines the END of the period/month under investigation:
                DateTo <- as.numeric(as.POSIXct("01/03/2015 00:00", tz = "", "%d/%m/%Y %H:%M")) 

        #MonthString is a string reflecting the MONTH under investigation

        #        datestr<-as.POSIXct("01/01/2015 00:00", tz = "", "%d/%m/%Y %H:%M")
                MonthString <- format(as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M"), "%B %Y")


#The following extracts the make up of Cobbling broken down by Service Item:       
        salesFrame <- lapply(1:length(salesFrame[,1]), FUN=replaceItems, Table = salesFrame)
        salesFrame <- ldply(salesFrame, data.frame)

        CobblingData=salesFrame[(salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom)&(salesFrame$ServiceDescription=='Cobbling'),]
        CobblingData$ServiceItem <- gsub("Cob - ", "", CobblingData$ServiceItem)
        CobblingData$ServiceItem <- gsub("Misc - ", "", CobblingData$ServiceItem)
        CobblingData$ServiceItem <- gsub(" - Each", "", CobblingData$ServiceItem)
        CobblingData$ServiceItem <- gsub(" 1/2", "", CobblingData$ServiceItem)

                aggCobblingSTTableTF <- aggregate(Value ~ ServiceItem, FUN = "sum", data=CobblingData)        
                aggCobblingSTTableTF$ServiceItem <- gsub("Cob - ", "", aggCobblingSTTableTF$ServiceItem)
                aggCobblingSTTableTF$ServiceItem <- gsub("Misc - ", "", aggCobblingSTTableTF$ServiceItem)
                aggCobblingSTTableTF <- setNames(aggCobblingSTTableTF, c("ServiceItem", "Value"))
                aggCobblingSTTableTF <- arrange(aggCobblingSTTableTF, -Value)
        
                EntireCobblingData=salesFrame[(salesFrame$ServiceItem==aggCobblingSTTableTF[9,1])&(salesFrame$ServiceDescription=='Cobbling'),]
                ECData=salesFrame[(salesFrame$ServiceDescription=='Cobbling'),]
                EntireaggCobblingSTTableTF <- aggregate(Value ~ yMon, FUN = "sum", data=EntireCobblingData,na.action=0) 
                EntireaggCobblingSTTableTF <- aggregate(Value ~ yMon+ServiceItem,  FUN = "sum", data=ECData) 

AggregateItemHistory <- function(serviceitemStr, dataFrame, categoryStr)
{
                # categoryStr = 'Cobbling' for instance
                # serviceitemStr = 'Cleaning' for instance
                subdataFrame = dataFrame[(dataFrame$ServiceItem==serviceitemStr)&(dataFrame$ServiceDescription==categoryStr),]
                
                itemHistory <- aggregate(Value ~ yMon, FUN = "sum", data = subdataFrame) 
                #itemHistory <- setNames(itemHistory[,2], c("serviceitemStr"))
                
                return(itemHistory[,2])
}
ItemHistoryFunction <- function(dataFrame,categoryStr,)
{
        itemList <- unique(CobblingData$ServiceItem)
        
        itemList <- unique(dataFrame$ServiceItem)
        newMonthTable<-lapply(itemList, FUN=AggregateItemHistory, dataFrame = salesFrame, categoryStr = 'Cobbling')
        newMonthTable<-sapply(itemList, FUN=AggregateItemHistory, dataFrame = salesFrame, categoryStr = 'Cobbling')
        newMonthTable <- ldply(newMonthTable, data.frame)
}
# -----------------------------------------------------------------------------------------------------
# We want to take data from a long format to wide format.
# EntireaggCobblingSTTableTF is in long format.  Its has column names yMon, ServiceItem and Value
# heading yMon has to make up the date column and ServiceItem the headings and Value the content of the table
# This is a try-out from (http://stackoverflow.com/questions/14749958/cast-a-data-frame-with-a-timevar-in-reshape2-as-with-the-reshape-base-function)
        reshape(data.frame(EntireaggCobblingSTTableTF), direction="wide",idvars="yMon")
        reshape(EntireaggCobblingSTTableTF, direction = "wide", idvars = c("yMon"))
        d2 <- melt(EntireaggCobblingSTTableTF, id.vars = c("ServiceItem"), timevar="yMon")
        dcast(d2, id ~ variable)
        dcast(EntireaggCobblingSTTableTF, id ~ value, value.var="Value")
# This almost works:
        dC <- dcast(EntireaggCobblingSTTableTF, yMon ~ ServiceItem, value.var = 'Value')

#-----------------------------------------------------------------------------------------------------
        aggAlterationsSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem, FUN = "sum", data=salesFrame[(salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom)&(salesFrame$ServiceDescription=='Alterations'),])        
        aggDryCleaningSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem, FUN = "sum", data=salesFrame[(salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom)&(salesFrame$ServiceDescription=='Dry-cleaning'),])        
        # Define data:
                Data = salesFrame[(salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom),]
                aggCustomerSTTableTF <- aggregate(x=data, by=list(Value,Customer), FUN = "sum", data = Data)        

        aggCustomerSTTableTF2 <- aggregate(as.numeric(data.matrix(Value)) ~ Customer, list(FUN = "length", data=salesFrame[(salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom),])        
        aggCustomerSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ Customer, FUN = "sum", data=salesFrame[(salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom),])        
                                           
        sqlQ3$wday <- as.POSIXlt(sqlQ3$SalesDate)$wday
        aggWeekDaySTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ wday, FUN = "sum", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom),])        
        
#Here we do staff Sales analysis on PRODUCTS:

        ProductData=productFrame[(productFrame$SalesDate<DateTo)&(productFrame$SalesDate>=DateFrom),]
        StaffProductSalesData <- aggregate(Value ~ UserName, FUN = "sum", data=ProductData)   
        StaffProductSalesData <- arrange(StaffProductSalesData, -Value)

        StaffCobblingSalesData <- aggregate(Value ~ UserName, FUN = "sum", data=CobblingData)   
        StaffCobblingSalesData <- arrange(StaffCobblingSalesData, -Value)
