#This script will extract data from the iLaundry database
#in a format ready to be exported Excel where it will be readied for import in bulkSMS
#The excel spreadsheet is called BulkSMSCostomerFeedbackSheet (Autosaved).xlsx for now
# and can be found at C:\DOC\Levingers
#('www.bulksms.co.za, LevingersVP, BulkSMS!')
#The upload file can be found at c:\doc\Levingers  Look for uploadBULKSMS.txt

library(RODBC)

f<-function(x){ y<- strsplit(as.character(x)," ", fixed = TRUE)
                d<-paste(y[[1]][1], y[[1]][length(y[[1]])], sep = ",", collapse = NULL)
                return(d)}

DateF <- as.POSIXct("01/12/2014 00:00", tz = "", "%d/%m/%Y %H:%M")
DateT <- as.POSIXct("31/01/2015 00:00", tz = "", "%d/%m/%Y %H:%M")
DateZ <- as.POSIXct("01/10/2014 00:00", tz = "", "%d/%m/%Y %H:%M")

DateFrom <- as.numeric(DateF)        #The unique ServiceItem values are found so:
DateTo <- as.numeric(DateT)        
DateZero <- as.numeric(DateZ)        #The unique ServiceItem values are found so:

ch <- odbcConnect("SystemHData")

Str1 <- 'SELECT SALESDOCUMENT.DOCUMENTNO as Invoice, 
                iif(CUSTOMERRETAIL.CUSTOMERTITLEID =8,
                \'\',CUSTOMERTITLE.DESCRIPTION)+\' \'+  
                CUSTOMERRETAIL.FIRSTNAME+\' \'+
                CUSTOMERRETAIL.SURNAME as Customer,  
                CUSTOMERRETAIL.MOBILENUMBER as Mobile,
                SALESDOCUMENT.DOCUMENTDATE as SalesDate
        FROM 
                CUSTOMERRETAIL INNER JOIN SALESDOCUMENT ON 
                CUSTOMERRETAIL.CUSTOMERRETAILID = SALESDOCUMENT.CUSTOMERRETAILID INNER JOIN
                CUSTOMERTITLE ON CUSTOMERRETAIL.CUSTOMERTITLEID = CUSTOMERTITLE.CUSTOMERTITLEID INNER JOIN
                SALESDOCUMENTLINE ON SALESDOCUMENT.SALESDOCUMENTID = SALESDOCUMENTLINE.SALESDOCUMENTID
        WHERE  (SALESDOCUMENT.DOCUMENTTYPEID = 2) AND 
                (SALESDOCUMENT.DOCUMENTSTATUSID = 10) AND 
                (SALESDOCUMENTLINE.SERVICEITEMTYPEID IS NOT NULL)
        GROUP BY SALESDOCUMENT.DOCUMENTDATE,SALESDOCUMENT.DOCUMENTNO, CUSTOMERRETAIL.SURNAME, 
                CUSTOMERRETAIL.FIRSTNAME, 
                CUSTOMERRETAIL.MOBILENUMBER, 
                CUSTOMERRETAIL.CUSTOMERTITLEID, 
                CUSTOMERTITLE.DESCRIPTION'
                                               
#Str2<- 'use iLaundry SELECT * FROM SALESDOCUMENT'
#Str2 <- 'SELECT *, SUM(Value) OVER(partition by ServiceDescription order by  CompletedDate)[Cumulative Sum]
#FROM #myTempTable'

##sqlQ2 <- sqlQuery(ch, paste(Str2))
        BulkSMS <- sqlQuery(ch, paste(Str1))
        odbcClose(ch)
#This line removes all Counter Stock Items
        BulkSMS <- BulkSMS[!BulkSMS$Customer==' COUNTER STOCK',]


exclList <- (DateZero <= BulkSMS$SalesDate) & (DateFrom > BulkSMS$SalesDate)
inclList <- (DateFrom <= BulkSMS$SalesDate) & (DateTo >= BulkSMS$SalesDate)
thismonthBulkSMS <- BulkSMS[inclList,]
previousBulkSMS <- BulkSMS[exclList,]

#These are the customers who visited during the past month but ALSO
# during the two months prior to that:
        repeatList <- intersect(BulkSMS$Mobile[exclList],BulkSMS$Mobile[inclList])
# The following is a list of those customers who
# inclBulkSMS <- BulkSMS[inclList, ]
inclVector <- !is.element(BulkSMS$Mobile[inclList], BulkSMS$Mobile[exclList])
exclBulkSMS <- thismonthBulkSMS[inclVector, ]
repeatBulkSMS <- thismonthBulkSMS[!inclVector, ]
Mobile<-gsub("[[:punct:]]", "", as.character(exclBulkSMS$Mobile))
CustList <- lapply(exclBulkSMS$Customer, f)
BulkList <- paste(Mobile, CustList, sep = ",", collapse = NULL)

# finclVector <- is.element(BulkSMS$Mobile[inclList][inclVector], BulkSMS$Mobile)

# BulkSMS[inclVector,]
#currentList <- BulkSMS$Mobile[inclList][inclVector]
filename <- paste("c:/rProgramming/ShopStats/RAWBulkSMS ",DateF," to ", DateT,".csv",sep="")
write.csv(file=filename, x = BulkList,  row.names = FALSE)

read.csv(file="c:/rProgramming/ShopStats/RAWBulkSMS.csv", 
