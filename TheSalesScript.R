# This script reports on Sales 
# How did we make our money this month along Service Descriptions and
# The top 15 Service types
# Which service types gave 80% of our turnover...
# The number of NEW custies
# The number of RETURNING custies
# How many days on average since we saw the returning custies
# How many completed orders?
# How many Sales orders?

        setwd("C:/rProgramming/ShopStats/Graphs")
        wd <- getwd()
        
# Lets do the formalities first:

# Load the following Libraries:

        library(timeDate)
        library(zoo)
        library(date)
        library(chron)
        library(reshape2)    
        library(ggplot2)           
        library(gridExtra)
        library(plyr)
        # install.packages("devtools")
        
        #library(devtools)
        #install_github("plotflow", "trinker")        
#  The start and end dates for SEPTEMBER 2014:
        DateZero <- as.numeric(as.POSIXct("01/01/2013 00:00", tz = "", "%d/%m/%Y %H:%M")) 
        DateFrom <- as.numeric(as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
        DateTo <- as.numeric(as.POSIXct("01/03/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))  
                datestr<-as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M")
                MonthString <- format(datestr, "%B %Y")
        
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
        
        
        #UniqueSalesOrders <- unique(sqlQ3[IndexToFrom,]$Invoice)
        #NumberReturningCustomers <- length(intersect(unique(sqlQ3[IndexToFrom,]$CustomerNo),unique(sqlQ3[IndexZeroFrom,]$CustomerNo)))
        #NumberofCustomers <- length(unique(sqlQ3[IndexToFrom,]$CustomerNo))
        #NumberNewCustomers <- NumberofCustomers - NumberReturningCustomers
   
         
## This script connects to the latest iLaundry db on this computer
        # ch <- odbcConnect("SystemHData")
        # Close connection to the DB like so:     
        # odbcClose(ch)       
# Remove myTempTable 
#         Str0 <- 'drop table #myTempTable'
#         sqlQuery(ch, paste(Str0))
# Done
        
        
#This string is to produce a query for SERVICE rendered of ANY type...
        chShop <- odbcConnect("shopData")
                salesFrame <- sqlFetch(chShop, "salesFrame")
                salesFrame$SalesDate<-as.POSIXct(as.Date(salesFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
        odbcClose(chShop) 
        
# This line returns no result, Result is read into MyTempTable        
        sqlQ1 <- sqlQuery(ch, paste(SalesOrdersSQLstring))

# This result reads the result from MyTempTable        
#         SalesOrdersSQLstring2 <- 'SELECT *, SUM(Value) OVER(partition by ServiceDescription order by  SalesDate)[Cumulative Sum]
#         FROM #myTempTable'

        
        SalesOrders<- sqlQuery(ch, paste(SalesOrdersSQLstring2))  
        
        Str3 <- 'SELECT * FROM #myTempTable'
        sqlQ3<- sqlQuery(ch, paste(Str3))
        newlsqlQ3 <- lapply(1:length(sqlQ3[,1]), FUN=replaceItems, Table = sqlQ3)
        sqlQ3 <- ldply(newlsqlQ3, data.frame)
        odbcClose(ch)  
        
        #sqlQ3 <- newlsqlQ3
#         DateFrom <- as.numeric(as.POSIXct("01/12/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
#         DateTo <- as.numeric(as.POSIXct("01/01/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))        
        # unique(floor(unclass(as.numeric(as.POSIXct(sqlQ3$SalesDate, tz = "", "%d/%m/%Y")))/86400))
        uniqueDays <- unique(as.Date(salesFrame$SalesDate))
        
        
#This is the index for values between the From and To dates, the Current period:
        IndexToFrom <- (salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom)
        
#This is the index for values between the Zero and From dates, the prior period:        
        IndexZeroFrom <- (salesFrame$SalesDate<DateFrom)&(salesFrame$SalesDate>=DateZero)
        
#This is the index for values between the Zero and To dates, the Total period:        
        IndexZeroTo <- (salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateZero)
        
        # ch <- odbcConnect("Swimming")
        # eventFrame<-sqlFetch(ch, "eventFrame", as.is= TRUE)
        # odbcClose(ch)   
        dataFrame <- salesFrame[IndexToFrom,]
        
#The following calculates the number of unique sales orders for this month:        
        UniqueSalesOrders <- unique(dataFrame$Invoice)
        NumberReturningCustomers <- length(intersect(unique(salesFrame[IndexToFrom,]$CustomerNo),unique(salesFrame[IndexZeroFrom,]$CustomerNo)))
        NumberofCustomers <- length(unique(dataFrame$CustomerNo))
        NumberNewCustomers <- NumberofCustomers - NumberReturningCustomers
        ## ceiling(length(uniqueDays)/7)
        dm <- matrix(data = rep(1:ceiling(length(uniqueDays)/7),7), nrow = ceiling(length(uniqueDays)/7), ncol = 7, byrow = FALSE, dimnames = NULL)
        dm <- matrix(data = rep(1:7,ceiling(length(uniqueDays)/7)), nrow = ceiling(length(uniqueDays)/7), ncol = 7, byrow = TRUE, dimnames = NULL)
        ##array(data=dm)
        # this gives th
        weekarray <- rep(1:ceiling(length(uniqueDays)/7),each=7)
        
        analysisArray <- lapply(uniqueDays, DailyCustomerAnalysis, salesFrame)
        dmat <- matrix(data=unlist(analysisArray),ncol=3, byrow=TRUE)
        cbind(rep(1:ceiling(length(uniqueDays)/7),each=7),uniqueDays, dmat)
        
        aggSDTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceDescription, FUN = sum, data=sqlQ3[IndexToFrom,])        
        aggSDTableTF <- aggSDTableTF[order(-aggSDTableTF[,2]),]
        write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/ServiceTableSeptember2014.csv", x=aggSDTableTF)
        
#The following extracts the make up of Cobbling broken down by Service Item:       
        
        aggCobblingSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem, FUN = "sum", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Cobbling'),])        
        aggAlterationsSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem, FUN = "sum", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Alterations'),])        
        aggDryCleaningSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ ServiceItem, FUN = "sum", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Dry-cleaning'),])        
        aggCustomerSTTableTF <- aggregate(as.numeric(data.matrix(Value)) ~ Customer, FUN = "sum", data=sqlQ3[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom),])        
        
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
        aggCobblingSTTableTF <- arrange(aggCobblingSTTableTF, -Value)

        write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/CobblingTableSeptember2014.csv", x=aggCobblingSTTableTF)
        
        aggAlterationsSTTableTF <- setNames(aggAlterationsSTTableTF, c("ServiceItem", "Value", "Percentage", "CumulativePerc"))
        aggAlterationsSTTableTF <- transform(aggAlterationsSTTableTF, ServiceItem=reorder(ServiceItem, Value) ) 
        aggAlterationsSTTableTF80 <- aggAlterationsSTTableTF[aggAlterationsSTTableTF$CumulativePerc<0.80,]
        aggAlterationsSTTableTF <- arrange(aggAlterationsSTTableTF, -Value)
        write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/AlterationsTableSeptember2014.csv", x=aggAlterationsSTTableTF)
        
        aggDryCleaningSTTableTF <- setNames(aggDryCleaningSTTableTF, c("ServiceItem", "Value", "Percentage", "CumulativePerc"))
        aggDryCleaningSTTableTF <- transform(aggDryCleaningSTTableTF, ServiceItem=reorder(ServiceItem, Value) ) 
        aggDryCleaningSTTableTF <- arrange(aggDryCleaningSTTableTF, -Value)
        aggDryCleaningSTTableTF80 <- aggDryCleaningSTTableTF[aggDryCleaningSTTableTF$CumulativePerc<0.80,]
        write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/DryCleaningTableSeptember2014.csv", x=aggDryCleaningSTTableTF)
        
        #ServiceValue = sqlQ3$Value[(sqlQ3$SalesDate<DateTo)&(sqlQ3$SalesDate>=DateFrom)&(sqlQ3$ServiceDescription=='Cobbling')])),)
#And sort it:
#        aggCobblingSTTableTF <- aggCobblingSTTableTF[order(-aggCobblingSTTableTF[,2]),]
        #c <- qplot(ServiceItem, Value, data = aggCobblingSTTableTF80)
#        aggCobblingSTTableTF$ServiceItem <- factor(aggCobblingSTTableTF$ServiceItem, levels=aggCobblingSTTableTF[order(-aggCobblingSTTableTF$Value), "ServiceItem"])
#        aggCobblingSTTableTF80$ServiceItem <- factor(aggCobblingSTTableTF$ServiceItem, levels=aggCobblingSTTableTF[order(-aggCobblingSTTableTF$Value), "ServiceItem"])
# Here are the graffs ---------------------------------------------------------------- 

ServicePLOTfunction(TableTF = aggCobblingSTTableTF, 
                    FileDescriptionString = "Levingers Victory Park - Cobbling - ", 
                    DepartmentString = "Cobbling",
                    MonthString = MonthString, 
                    FillColour = "navy")

ServicePLOTfunction(TableTF = aggAlterationsSTTableTF, 
                    FileDescriptionString = "Levingers Victory Park - Alterations - ", 
                    DepartmentString = "Alterations",
                    MonthString = MonthString, 
                    FillColour = "yellow")

ServicePLOTfunction(TableTF = aggDryCleaningSTTableTF, 
                    FileDescriptionString = "Levingers Victory Park - DryCleaning - ", 
                    DepartmentString = "DryCleaning",
                    MonthString = MonthString, 
                    FillColour = "pink")


        png(file=paste("Levingers Victory Park - Cobbling - ", MonthString,".png"))                
        x <- ggplot(data = aggCobblingSTTableTF, 
                        mapping = aes(x = ServiceItem, y = Value)) +
                        geom_bar(colour="black", fill="red", width=.7, stat="identity") +
                                labs(title = paste("Cobbling \n",MonthString), 
                                     x=paste(" Service Item \n",""), 
                                     y=paste("Rand \n",""))+ 
                                theme(
                                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                                        axis.text = element_text(size = rel(1), colour = "Black"),
                                        #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                                        #panel.grid.minor = element_blank(), 
                                        #panel.grid.major = element_blank(),
                                        #legend.title = element_text("This is it!"),
                                        #                plot.background = element_rect(fill = "white",colour = NA)
                                        plot.background = element_rect(fill = "transparent",colour = NA)
                                #axis.text = element_text(colour="yellow")
                                        ) +
                coord_flip()
        x + ggtitle(paste("Levingers Victory Park - Cobbling - ", MonthString)
                    
                    ServicePLOTfunction(TableTF, MonthString)                    
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
        
#         
#         png(file=paste("Time Comparison -", 
#                        m1$DistanceString.x[1], 
#                        m1$StrokeString.x[1], "vs", 
#                        m1$DistanceString.y[1], 
#                        m1$StrokeString.y[1],
#                        m1$EventType.y[1],
#                        ".png"))        
#         
#         
#         compplot<-ggplot(m1, mapping = aes(x=Seconds.x, y=Seconds.y,size=AgeGroup.x,
#                                            label=Name.x, colour=Gender.x))+
#                 geom_point(data= m1,aes(title = "Event"), alpha=0.5)+
#                 #geom_point(data= m1,aes(shape=EventType.x,name = "Event"), alpha=0.5)+
#                 #geom_point(aes(colour = Province)) +
#                 labs(title = "Time Comparison", 
#                      x=paste("Time (s) \n",m1$EventString.x[1]), 
#                      y=paste("Time (s) \n",m1$EventString.y[1]),
#                      colour = "Gender") +#scale_size_area(min_size = 1,max_size = 10)+
#                 geom_text(size=2.5, aes(label=Name.x),hjust=-0.15, vjust=0, colour="black") +
#                 scale_size(range = c(1, 15), name = "Relative Age")+  
#                 guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
#                 theme(
#                         axis.title.y = element_text(size = rel(1.25), colour = "Black"),
#                         axis.title.x = element_text(size = rel(1.25), colour = "Black"),
#                         plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
#                         axis.text = element_text(size = rel(1), colour = "Black"),
#                         #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
#                         #panel.grid.minor = element_blank(), 
#                         #panel.grid.major = element_blank(),
#                         #legend.title = element_text("This is it!"),
#                         #                plot.background = element_rect(fill = "white",colour = NA)
#                         plot.background = element_rect(fill = "transparent",colour = NA)
#                         #axis.text = element_text(colour="yellow")
#                 ) +
#                 guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))#+
#         makeHeadnote(HeadNoteALL, color = "black")
#         print(compplot)
#         dev.off()
#         