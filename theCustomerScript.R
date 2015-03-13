# Remember to RUN the StartUp.R script FIRST.
# Set the working directory like so:
        wd <- getwd()
        setwd("C:/rProgramming/ShopStats/Graphs")

# Lets get the data like this and format it like so:
chShop <- odbcConnect("shopData")
        salesFrame <- sqlFetch(chShop, "salesFrame")
        salesFrame$SalesDate<-as.POSIXct(as.Date(salesFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
        salesFrame$SalesDateC<-as.Date(salesFrame$SalesDate)
        
        salesFrame$Value<-as.numeric(data.matrix(salesFrame$Value))
        salesFrame$ServiceItem <- gsub("Cob - ", "", salesFrame$ServiceItem)
        salesFrame$ServiceItem <- gsub("Misc - ", "", salesFrame$ServiceItem)
        salesFrame$ServiceItem <- gsub(" - Each", "", salesFrame$ServiceItem)
        salesFrame$ServiceItem <- gsub(" 1/2", "", salesFrame$ServiceItem)
odbcClose(chShop)

#  The start and end dates for SEPTEMBER 2014:
        DateZero <- (as.POSIXct("01/01/2013 00:00", tz = "", "%d/%m/%Y %H:%M")) 
        DateFrom <- (as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
        DateTo <- (as.POSIXct("01/03/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))  
        datestr<-as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M")
        MonthString <- format(datestr, "%B %Y")


#This is the index for values between the From and To dates, the Current period:
#IndexToFrom <- (salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom)
        IndexToFrom <- (as.Date(salesFrame$SalesDate)<as.Date(DateTo))&(as.Date(salesFrame$SalesDate)>=as.Date(DateFrom))

#This is the index for values between the Zero and From dates, the prior period:        
        IndexZeroFrom <- (as.Date(salesFrame$SalesDate)<as.Date(DateFrom))&(as.Date(salesFrame$SalesDate)>=as.Date(DateZero))

#This is the index for values between the Zero and To dates, the Total period:        
        IndexZeroTo <- (as.Date(salesFrame$SalesDate)<as.Date(DateTo))&(as.Date(salesFrame$SalesDate)>=as.Date(DateZero))



DailyCustomerAnalysis <- function(Day1, Frame){
        #       unique(as.Date(sqlQ3$SalesDate))        
        DateZero <- as.numeric(as.POSIXct("01/01/2013 00:00", tz = "", "%d/%m/%Y %H:%M")) 
        DateFrom <- as.numeric(as.POSIXct(as.Date(Day1), tz = "", "%d/%m/%Y"))        #The unique ServiceItem values are found so:
        DateTo <- as.numeric(as.POSIXct(as.Date(Day1)+1, tz = "", "%d/%m/%Y"))
        
        IndexToFrom <- (Frame$SalesDate<DateTo)&(Frame$SalesDate>=DateFrom)
        IndexZeroFrom <- (Frame$SalesDate<DateFrom)&(Frame$SalesDate>=DateZero)
        UniqueSalesOrders <- unique(Frame[IndexToFrom,]$Invoice)
        
        NumberReturningCustomers <- length(intersect(unique(Frame[IndexToFrom,]$CustomerNo),unique(Frame[IndexZeroFrom,]$CustomerNo)))
        NumberofCustomers <- length(unique(Frame[IndexToFrom,]$CustomerNo))
        NumberNewCustomers <- NumberofCustomers - NumberReturningCustomers
        Date <- as.Date(Day1)
        X <-data.frame(Date,NumberReturningCustomers,NumberNewCustomers,NumberofCustomers)
        
        #        DateFrom <- as.numeric(as.Date(Day1), tz = "", "%d/%m/%Y")        #The unique ServiceItem values are found so:
        #        DateTo <- as.numeric(as.Date(Day1)+1, tz = "", "%d/%m/%Y") 
return(X)}

        UniqueSalesOrders <- unique(salesFrame$Invoice)
        NumberReturningCustomers <- length(intersect(unique(salesFrame[IndexToFrom,]$CustomerNo),unique(salesFrame[IndexZeroFrom,]$CustomerNo)))
        NumberofCustomers <- length(unique(salesFrame$CustomerNo[IndexToFrom]))
        NumberNewCustomers <- NumberofCustomers - NumberReturningCustomers
## ceiling(length(uniqueDays)/7)
        dm <- matrix(data = rep(1:ceiling(length(uniqueDays)/7),7), nrow = ceiling(length(uniqueDays)/7), ncol = 7, byrow = FALSE, dimnames = NULL)
        dm <- matrix(data = rep(1:7,ceiling(length(uniqueDays)/7)), nrow = ceiling(length(uniqueDays)/7), ncol = 7, byrow = TRUE, dimnames = NULL)
##array(data=dm)
# this gives th
        weekarray <- rep(1:ceiling(length(uniqueDays)/7),each=7)
        uniqueDays <- unique(as.Date(salesFrame$SalesDate))
        
        analysisArray <- lapply(uniqueDays, DailyCustomerAnalysis, salesFrame)
#         dmat <- matrix(data=unlist(analysisArray),ncol=3, byrow=TRUE)
#         cb <- cbind(rep(1:ceiling(length(uniqueDays)/7),each=7),uniqueDays, dmat)
        analysisArraydf <- ldply(analysisArray, data.frame)
        
        aggCobblingSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ (ServiceType+SalesDateC), FUN = "sum", data=salesFrame)        
        