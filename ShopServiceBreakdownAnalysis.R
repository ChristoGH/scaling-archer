# This function requires the script ServiceBreakdown.Rto have run
# for starter we will be working with sqlQ2 only.
# Inspect the header names of the file first:
# Load the following Libraries:

wd <- getwd()
setwd("C:/rProgramming/ShopStats/Graphs")


chShop <- odbcConnect("shopData")
        salesFrame <- sqlFetch(chShop, "salesFrame")
#         productFrame <- sqlFetch(chShop, "productFrame")
#         salesFrame$SalesDate<-as.POSIXct(as.Date(salesFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
#         productFrame$SalesDate<-as.POSIXct(as.Date(productFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
# 
odbcClose(chShop) 


# We want to plot the unique values found under column ServiceDescription and ServiceItem by Day Week and Month

#The unique ServiceDescription values are found so:

        uniqueSD <- unique(salesFrame$ServiceDescription)
        DateFrom <- as.numeric(as.POSIXct("01/12/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
        DateTo <- as.numeric(as.POSIXct("01/01/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))        
        IndexToFrom <- (salesFrame$CompletedDate>=DateFrom) & (salesFrame$CompletedDate<DateTo)   
        uniqueSI <- unique(salesFrame$ServiceItem)


        newFrame <- salesFrame[salesFrame$ServiceDescription=='Alterations',]
# The following line of code converts sd to an array of date elements 
# where each element only contain a MONTH and a year.       
        yMon <- as.yearmon(as.Date(salesFrame$CompletedDate), "%YM%m")
#        seq(as.Date(sqlQ2$CompletedDate), length=24, by="1 month") - 1
#this is the list of unique yearMonth strings as per the database:
        uniqueYM <- unique(yMon)


dvec <- as.Date(salesFrame$CompletedDate)
dweek <- as.numeric(dvec-dvec[1]) %/% 7 + 1
#dweek[1:21]
# [1] 0 0 0 0 0 0 0 1 1 1 1 1 1 1 2 2 2 2 2 2 2
#This vector of STRING do not include TIME:

uniqueDAYS <- strftime(salesFrame$CompletedDate, tz = "GMT", "%Y-%m-%d")
aggregate(as.numeric(Value) ~ ServiceDescription, FUN = sum, data=sqlQ2)

        aggSITable <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem, FUN = sum, data=SalesOrders)
        aggSDTable <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceDescription, FUN = sum, data=SalesOrders)
        aggSDTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceDescription, FUN = sum, data=sqlQ3[IndexToFrom,])
        
#Here is the table that contains the top 15 contributors to TurnOver        
        aggSITable <- aggSITable[order(-aggSITable[,2]),]
#Here is a table of the top15 contributors to Turnover by WEEK:
        aggWeek15 <- aggSITable[1:15,]
# Here is the INDEX into the DB of the top 15 performers        
        top15Index <- match(sqlQ2$ServiceItem, aggWeek15[,1])
# Here is the abridged db of the top 15 performers        
        sqlQ2TOP15 <- sqlQ2[!is.na(matchWeek15),]
#The following is the date vector on which top 15 performers were completed.        
        dvec15 <- as.Date(sqlQ2TOP15$CompletedDate)
# The following array is a week count number since the start of the db:
        dweek15 <- as.numeric(dvec15-dvec15[1]) %/% 7 + 1
      
        
        
##  This is tggregate(as.numeric(Value) ~ ServiceDescription + as.character(uniqueDAYS), FUN = sum, data=sqlQ2)


#The following yields a frame showing aggreagated COMPLETED Value by ServiceItem by DAY:
        aggSIDay <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem + as.character(uniqueDAYS), FUN = sum, data=sqlQ2)
        aggSIDaydf1 <- data.frame(ServiceItem = aggSIDay$ServiceItem, Date = aggSIDay[,2], Value = aggSIDay[,3])
        d <- ggplot(data=aggSIDaydf1, aes(x=Date, fill=ServiceItem, stat="identity"))   
        d + geom_bar()
        
#The following yields a frame showing aggreagated COMPLETED Value by ServiceItem by WEEK:
        aggSIWeek <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem + dweek, FUN = sum, data=sqlQ2)
        aggSIWeekdf1 <- data.frame(ServiceItem = aggSIWeek$ServiceItem, Week = aggSIWeek[,2], Value = aggSIWeek[,3])
        d <- ggplot(data=aggSIWeekdf1, aes(x=Week, y=Value, fill=ServiceItem, stat="identity"))   
        plot2 <- d + geom_bar(stat="identity")
        plot2
        
# The following now plots the top overall per performing service Items per week
        aggSIWeekTOP15 <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem + dweek15 + ServiceDescription, FUN = sum, data=sqlQ2TOP15)
        aggSIWeekTOP15df1 <- data.frame(ServiceItem = aggSIWeekTOP15$ServiceItem, Week = aggSIWeekTOP15[,2], ServiceDescription = aggSIWeekTOP15$ServiceDescription, Value = aggSIWeekTOP15[,4])
        d <- ggplot(data=aggSIWeekTOP15df1, aes(x=Week, y=Value, fill=ServiceDescription, stat="identity"))   
        plot3 <- d + geom_bar(stat="identity")
        plot3
#        g_legend<-function(a.gplot){
#                tmp <- ggplot_gtable(ggplot_build(a.gplot))
#                leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#                legend <- tmp$grobs[[leg]]
#                legend
#        }
        
#        legend <- g_legend(plot2)
#        legend
#        grid.arrange(legend, plot2+ theme(legend.position = 'none'), 
#                     ncol=2, nrow=1, widths=c(1/6,5/6))        
        
        
#The following yields a frame showing aggreagated COMPLETED Value by ServiceItem by MONTH:
        aggSIMonth <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem + yMon, FUN = sum, data=sqlQ2)

#The following yields a frame showing aggreagated COMPLETED Value by ServiceItem:

        aggSI<-aggregate(as.numeric(sqlQ2$Value) ~ ServiceItem, FUN = sum, data=sqlQ2, simplify = TRUE)

#The following gives a break down in a data frame of aggregated COMPLETED Values by Service type for the TOTAL period under review..
        df1 <- data.frame(ServiceItem = aggSI$ServiceItem, Value = aggSI[,2])
        
        d <- ggplot(data=aggSIDay, aes(x=yMon, y=aggSIMonth[,3], colour=ServiceItem))       
        d + geom_bar()
        pd <- position_dodge(.1)
        geom_line(position=pd,aes(group=ServiceItem))        
        
        
        as.Date(strptime(yMon, format = "%Y M%m"))
        next.month <- function(d) as.Date(as.yearmon(d) + 1/12) + 
                as.numeric(d - as.Date(as.yearmon(d)))
