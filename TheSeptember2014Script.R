# This script will report 7 key concepts for the shoppe
# How did we make our money this month along Service Descriptions and
# The top 15 Service types
# Which service types gave 80% of our turnover...
# The number of NEW custies
# The number of RETURNING custies
# How many days on average since we saw the returning custies
# How many completed orders?
# How many Sales orders?


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
        DateFrom <- as.numeric(as.POSIXct("01/09/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
        DateTo <- as.numeric(as.POSIXct("01/10/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        


# This invokes the ODBC package to call SQL
        library(RODBC)

## This script connects to the latest iLaundry db on this computer
        ch <- odbcConnect("SystemHData")

# Remove myTempTable 
        Str0 <- 'drop table #myTempTable'
        sqlQuery(ch, paste(Str0))
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
        
        
        sqlQ1 <- sqlQuery(ch, paste(SalesOrdersSQLstring))

        SalesOrdersSQLstring2 <- 'SELECT *, SUM(Value) OVER(partition by ServiceDescription order by  SalesDate)[Cumulative Sum]
        FROM #myTempTable'
        
        SalesOrders<- sqlQuery(ch, paste(SalesOrdersSQLstring2))  
        
        Str3 <- 'SELECT * FROM #myTempTable'
        sqlQ3<- sqlQuery(ch, paste(Str3)) 
#The following calculates the number of unique sales orders for the month:        
        UniqueSalesOrders <- unique(sqlQ3[IndexToFrom,]$Invoice)
        
        IndexToFrom <- (sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)
        aggSDTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceDescription, FUN = sum, data=sqlQ3[IndexToFrom,])        
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
        
        rep(1:7,6)
        ?matrix
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
