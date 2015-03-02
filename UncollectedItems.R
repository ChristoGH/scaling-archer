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
library(sqldf)

# install.packages("devtools")

#library(devtools)
#install_github("plotflow", "trinker")        
#  The start and end dates for SEPTEMBER 2014:
DateFrom <- as.numeric(as.POSIXct("01/10/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
DateTo <- as.numeric(as.POSIXct("01/1/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        


# This invokes the ODBC package to call SQL
library(RODBC)

## This script connects to the latest iLaundry db on this computer
ch <- odbcConnect("SystemHData")
UncollectedDF <- data.frame(c("S0001345", "S0001900"))
UncollectedDF <- setNames(UncollectedDF, c("UncollectedInvoice"))

# Remove myTempTable 
Str0 <- 'drop table #myTempTable'
sqlQuery(ch, paste(Str0))

dryCleaningList <- paste('\'S0001345\',',
                                        '\'S0001900\',',
                                        '\'S0002087\',',
                                        '\'S0002661\',',
                                        '\'S0003317\',',
                                        '\'S0003657\',',
                                        '\'S0002661\',',
                                         '\'S0003758\',',
                                         '\'S0003860\',',
                                         '\'S0004027\',',
                                         '\'S0004151\',',
                                         '\'S0004484\',',
                                         '\'S0004589\',',
                                         '\'S0005297\',',
                                         '\'S0005320\',',
                                         '\'S0005870\',',
                                         '\'S0005685\',',
                                         '\'S0005710\',',
                                         '\'S0004175\',',
                                         '\'S0004337\',',
                                         '\'S0002377\',',
                                         '\'S0004344\',',
                                         '\'S0003464\',',
                                         '\'S0003461\',',
                                         '\'S0003270\',',
                                         '\'S0004410\',',
                                         '\'S0004698\',',
                                         '\'S0002750\',',
                                         '\'S0004738\',',
                                         '\'S0004394\',',
                                         '\'S0005367\'')

#This string is to produce a query for SERVICE rendered of ANY type...
SalesOrdersSQLstring <-paste('use iLaundry SELECT SALESDOCUMENT.SALESDOCUMENTID as SalesDocumentID, 
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
                             WHERE (SALESDOCUMENT.DOCUMENTNO IN (',dryCleaningList,'))
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
SERVICEITEMTYPE.DESCRIPTION')

#AND (SALESDOCUMENT.DOCUMENTNO IN (\'S0001900\'))
sqlQ1 <- sqlQuery(ch, paste(SalesOrdersSQLstring))

UncollectedDryCleaningSQLstring2 <- 'SELECT *, SUM(Value) OVER(partition by ServiceDescription order by  SalesDate)[Cumulative Sum]
FROM #myTempTable'

UncollectedDryCleaning <- sqlQuery(ch, paste(UncollectedDryCleaningSQLstring2))
odbcClose(ch)   

f<-function(x){ y<- strsplit(as.character(x)," ", fixed = TRUE)
                d<-paste(y[[1]][1], y[[1]][length(y[[1]])], sep = ",", collapse = NULL)
                return(d)}

Mobile<-gsub("[[:punct:]]", "", as.character(UncollectedDryCleaning$Mobile))
CustList <- lapply(UncollectedDryCleaning$Customer, f)
BulkList <- paste(Mobile, CustList, sep = ",", collapse = NULL)

# sb <- (strsplit(as.character(UncollectedDryCleaning$Customer), " ", fixed = TRUE))
# pd <- lapply(UncollectedDryCleaning$Customer, f)
# testd <- mapply(bulkListFunc, Mobile, pd)
#cbound<-cbind(Mobile,pd)
# ped <- lapply(cbound, g)
write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/UncollectedDryCleaning.csv", x = BulkList,  row.names = FALSE)


# Run to here for the complete list 

# This line removes all unwanted punctuation marks from the string below.
Mobile<-gsub("[[:punct:]]", "", as.character(UncollectedDryCleaning$Mobile))
title <- gsub("[:-:]", "", alapply(strsplit(x, ''), function(x) which(x == '2'))s.character(UncollectedDryCleaning$Customer))
grex <- gregexpr(pattern = ' ',UncollectedDryCleaning$Customer)

UncollectedDryCleaning$Mobile<-gsub("[:(:]", "", as.character(UncollectedDryCleaning$Mobile))
UncollectedDryCleaning$Mobile<-gsub("[:):]", "", as.character(UncollectedDryCleaning$Mobile))
BulkSMSUncollected<-data.frame(UncollectedDryCleaning$Mobile, UncollectedDryCleaning$Customer)
##==============================================================================================================
# write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/UncollectedDryCleaning.csv", x=UncollectedDryCleaning)

f<-function(x){ y<- strsplit(as.character(x)," ", fixed = TRUE)
                d<-paste(x[[1]][1], y[[1]][length(y[[1]])], sep = ",", collapse = NULL)
                return(d)}

g<-function(x){ d<-paste(x[1,1][1], x[1,2][1], sep = ",", collapse = NULL)
                return(d)}

bulkListFunc <- function(x,y){                
                d<-paste(x[[1]], y[[1]], sep = ",", collapse = NULL)
                return(d)}

a1Ina2 <- sqldf('SELECT * FROM SalesOrders WHERE Invoice = (SELECT Invoice FROM SalesOrders INTERSECT SELECT UncollectedInvoice FROM UncollectedDF)')

a1Ina3 <- sqldf('SELECT UncollectedInvoice FROM UncollectedDF')

a1Ina4 <- sqldf('SELECT Invoice FROM SalesOrders INTERSECT SELECT UncollectedInvoice FROM UncollectedDF')

