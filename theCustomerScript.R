# Remember to RUN the StartUp.R script FIRST.
# So far:
# Salesframe is the generic all-in summary of the iLaundry databse for SERVICES rendered.
# The purpose of this R Script is to produce some graphs of the iLaundry database.
# The focus is the most recent month.  This month is compared to the previous month but 
# also the previous year's calendar month.
# *** The first graph is to explore the growth of the database ***
        # Growth in the database is illustrated by graphing the number of new customers and returning customers
        # by month.  The varibale analysisMonthdf is the data frame that achieves or presents the data.

# *** The second graph explores customer appearances for the most recent calendar month ***
# Break the database up in the three core components that of Cobbling, Alterations and Dry Cleaning
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
        salesFrame$yMon <- as.yearmon(as.Date(salesFrame$SalesDate), "%YM%m")
        
odbcClose(chShop)
#-----------------------------------------------------------------------------------------------------        
# Here we create 3 frames, one for each of the SERVICES offered b y the shop:
        
        drycleaningFrame <- salesFrame[salesFrame$ServiceDescription=="Dry-cleaning",]        
        cobblingFrame <- salesFrame[salesFrame$ServiceDescription=="Cobbling",]           
        alterationsFrame <- salesFrame[salesFrame$ServiceDescription=="Alterations",]   
#-----------------------------------------------------------------------------------------------------        
#The following code analyses the customer overlap, that is how many customers used
#         Drycleaning and alterations (alterationsDrycleaningOVL), 
#         Drycleaning and Cobbling (cobblingDrycleaningOVL), 
#         Cobbling and Alterations (alterationsCobblingOVL) and
#         Cobbling, Alterations and Drycleaning (totalOVL):
        
                cobblingDrycleaningOVL<-intersect(drycleaningFrame$CustomerNo,cobblingFrame$CustomerNo)
                alterationsDrycleaningOVL<-intersect(drycleaningFrame$CustomerNo,alterationsFrame$CustomerNo)
                alterationsCobblingOVL<-intersect(cobblingFrame$CustomerNo,alterationsFrame$CustomerNo)
                totalOVL<-intersect(drycleaningFrame$CustomerNo,alterationsCobblingOVL)
                length(cobblingDrycleaningOVL)
                length(alterationsDrycleaningOVL)
                length(alterationsCobblingOVL)
                length(totalOVL)
        
        
        #  The start and end dates for SEPTEMBER 2014:
        DateZero <- as.numeric(as.POSIXct("01/01/2013 00:00", tz = "", "%d/%m/%Y %H:%M")) 
        DateFrom <- as.numeric(as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
        DateTo <- as.numeric(as.POSIXct("01/03/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))  
        
        datestr<-as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M")
        MonthString <- format(max(salesFrame$SalesDate), "%B %Y")


#This is the index for values between the From and To dates, the Current period:
#IndexToFrom <- (salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom)
        IndexToFrom <- ((salesFrame$SalesDate)<(DateTo))&((salesFrame$SalesDate)>=(DateFrom))

#This is the index for values between the Zero and From dates, the prior period:        
        IndexZeroFrom <- (as.Date(salesFrame$SalesDate)<as.Date(DateFrom))&(as.Date(salesFrame$SalesDate)>=as.Date(DateZero))

#This is the index for values between the Zero and To dates, the Total period:        
        IndexZeroTo <- (as.Date(salesFrame$SalesDate)<as.Date(DateTo))&(as.Date(salesFrame$SalesDate)>=as.Date(DateZero))

#-----------------------------------------------------------------------------------------------------        
#In this section of code we only look at what happened this month.  
#        And what we are looking for is Customers who use 
#        more than one of our services or perhaps even all of our services
        
        cobblingDrycleaningOVL<-intersect(drycleaningFrame[((drycleaningFrame$SalesDate)<(DateTo))&((drycleaningFrame$SalesDate)>=(DateFrom)),]$CustomerNo,cobblingFrame[((cobblingFrame$SalesDate)<(DateTo))&((cobblingFrame$SalesDate)>=(DateFrom)),]$CustomerNo)
        alterationsDrycleaningOVL<-intersect(drycleaningFrame[((drycleaningFrame$SalesDate)<(DateTo))&((drycleaningFrame$SalesDate)>=(DateFrom)),]$CustomerNo,alterationsFrame[((alterationsFrame$SalesDate)<(DateTo))&((alterationsFrame$SalesDate)>=(DateFrom)),]$CustomerNo)
        alterationsCobblingOVL<-intersect(cobblingFrame[((cobblingFrame$SalesDate)<(DateTo))&((cobblingFrame$SalesDate)>=(DateFrom)),]$CustomerNo,alterationsFrame[((alterationsFrame$SalesDate)<(DateTo))&((alterationsFrame$SalesDate)>=(DateFrom)),]$CustomerNo)
        totalOVL<-intersect(drycleaningFrame[((drycleaningFrame$SalesDate)<(DateTo))&((drycleaningFrame$SalesDate)>=(DateFrom)),]$CustomerNo,alterationsCobblingOVL)
        length(cobblingDrycleaningOVL)
        length(alterationsDrycleaningOVL)
        length(alterationsCobblingOVL)
        length(totalOVL)
        
        
#-----------------------------------------------------------------------------------------------------        
        # Here is where we have to calculate 
        # 1.    the number of days since the last visit
        # 2.    the number of times previously visit
        # 3.    the average days between visits
        
# The following line of code is used to show all instances where a customer from the current month
# has visited the shop before and on which lines of the DB        
# This line of code yields the date when a particular customer last visited.        
        lastVisit <- max(salesFrame$SalesDate[is.element(salesFrame[!IndexToFrom,]$CustomerNo,salesFrame[IndexToFrom,]$CustomerNo[3])])
        firstVisit <- min(salesFrame$SalesDate[is.element(salesFrame[!IndexToFrom,]$CustomerNo,salesFrame[IndexToFrom,]$CustomerNo[3])])
        daysSince <- salesFrame[IndexToFrom,]$SalesDate[3] - lastVisit
        
        salesFrame$SalesDate[is.element(salesFrame[!IndexToFrom,]$CustomerNo,salesFrame[IndexToFrom,]$CustomerNo[3])]
#The following is a sorted DATE list of ALL the dates a particular customer has come to the shop:        
        dateList<-sort(unique(as.Date(salesFrame$SalesDate[is.element(salesFrame$CustomerNo,salesFrame[IndexToFrom,]$CustomerNo[3])])))        
#so that the following line yeilds the avarage days between visits:
        meanDays <- mean(dateList[2:length(dateList)]- dateList[1:length(dateList)-1])
# How many UNIQUE visits did this custy make to the shop, since inception? :
        numberVisits <- length(dateList)
        
                #       Let say Day = 26 January 2015 or
                                Day <- as.Date("2015-01-01")
                                DateZero <- as.numeric(as.POSIXct("01/01/2013 00:00", tz = "", "%d/%m/%Y %H:%M")) 
                #       Then 
                                DateFrom <- as.numeric(as.POSIXct(Day, tz = "", "%d/%m/%Y"))       
                #       And
                                DateTo <- as.numeric(as.POSIXct(Day+1, tz = "", "%d/%m/%Y"))
                              IndexToFrom <- (salesFrame$SalesDate<DateTo)&(salesFrame$SalesDate>=DateFrom)
                              IndexZeroFrom <- (salesFrame$SalesDate<DateFrom)&(salesFrame$SalesDate>=DateZero)
                              UniqueSalesOrders <- unique(salesFrame[IndexToFrom,]$Invoice)
        
#                         NumberReturningCustomers <- length(intersect(unique(salesFrame[IndexToFrom,]$CustomerNo),unique(salesFrame[IndexZeroFrom,]$CustomerNo)))
#                         NumberofCustomers <- length(unique(Frame[IndexToFrom,]$CustomerNo))
#                         NumberNewCustomers <- NumberofCustomers - NumberReturningCustomers
        
# The following function calculates the number of customers, 
# the number of returning customers and the number of new customers on any given "day1" date
#----------------------------------------------------------------------------------------------------        
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
        analysisArraydf$yMon <- as.yearmon(as.Date(analysisArraydf$Date), "%YM%m")
        
#The data frame needs to be sorted by Date to remove glitches and to get it ready for PLOTTING:        
        analysisArraydf <- arrange(analysisArraydf, Date)        
        analysisArraydf$dbGrowth <- cumsum(analysisArraydf$NumberNewCustomers)
        analysisMonthdf <- aggregate(cbind(NumberReturningCustomers,NumberNewCustomers,NumberofCustomers)~yMon, FUN = "sum", data=analysisArraydf)        
        analysisMonthdf$Date <- as.Date(analysisMonthdf$yMon)
        # analysisMonthdf <- aggregate(x = analysisArraydf, by = list(), FUN = "sum")        
        aggregate(CustomerNo, FUN = "sum", salesFrame)        
        aggregate(as.numeric(data.matrix(Value)) ~ Customer, FUN = "sum", data=salesFrame)        
        aggregate(as.numeric(data.matrix(Value)) ~ Customer, FUN = "length", data=salesFrame)        
        aggregate(x=salesFrame, by=list(Customer), FUN = "length", )  
#-Growth of the database-------------------------------------------------------------------------------
        
        png(file=paste("growthOfTheLevVPdatabase_", MonthString,".png",sep=""))                
        compplot <- ggplot(data = analysisArraydf, 
                           mapping = aes(x = Date, y = dbGrowth)) +
                geom_bar(fill = "blue", width=.7, stat="identity", alpha=0.7) +
                labs(title = "Growth of the Victory Park dB", 
                     x="Date", 
                     y="All in Customers")+ 
                theme(
                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                        axis.text = element_text(size = rel(1), colour = "Black"),
                        plot.background = element_rect(fill = "transparent",colour = NA)
                ) 
        print(compplot)
        makeHeadnote(paste("Shop statistics for",MonthString), color = "black")
        dev.off()
        
# -- This tests for a line plot as opposed to a histogram plot
        
        ggplot(data = analysisArraydf, 
               mapping = aes(x = Date, y = dbGrowth)) +
                geom_line(color = "blue", size=2, stat="identity", alpha=0.7)

        
# -This function then plots customer visits:    Blue - all cutomers per month        
#                                                 Red - new
#                                                 Green - returning
        ggplot(data = analysisMonthdf, 
               mapping = aes(x = Date)) +
                geom_line(aes(y = NumberofCustomers, color = "Total"),  size = 1)+
                geom_line(aes(y = NumberNewCustomers, color = "New"), size = 1)+
                geom_line(aes(y = NumberReturningCustomers, color = "Returning"),  size = 1)+
                geom_point(aes(y = NumberReturningCustomers, color = "Returning"),  size = 3)+
                geom_point(aes(y = NumberNewCustomers, color = "New"),  size = 3)+
                geom_point(aes(y = NumberofCustomers, color = "Total"),  size = 3)+
                labs(title = paste("Customer Visits per Month","\n All Services"), x="Month", y="Monthly Customer Count")+
                scale_colour_manual("Break down", 
                                    breaks = c("Total", "New", "Returning"),
                                    values = c("red", "green", "blue")) +
                guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
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
                guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))
                makeHeadnote(paste("Shop statistics for",MonthString), color = "black")
        
        
        
#----------------------------------------------------------------------------------------------------
# The following code represents a month and allows shop turnover numbers 
#        to be plotted on each day of the month:
        
        
        start <- as.Date("2015-02-01")
        numdays <- 250
        
        weeknum <- function(date){
                z <- as.Date(date, format="%Y-%m-%d")
                as.numeric( format(z-1, "%U"))
        }
        
        
        dates <- data.frame(date=seq(start, length.out=numdays, by="1 day"))
        dates <- within(dates, {
                weeknum <- weeknum(date)
                month   <- format(date, "%m")
                weekday <- format(date, "%a")
                day     <- format(date, "%d")
        })
        
        dates$weekday <- factor(dates$weekday, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
        
        library(ggplot2)
        ggplot(dates, aes(x=weekday, y=weeknum, fill = day)) + 
                geom_tile(fill="blue", col="white") +
                geom_text(aes(label=day)) + scale_colour_gradient(limits=c(1, 40))   

        ggplot(dates, aes(x=weekday, y=weeknum, fill = day)) + 
                geom_tile() +
                geom_text(aes(label=day)) + scale_colour_gradient(limits=c(1, 10))   

        
#-----------------------------------------------------------------------------------------------------
# Now merge the dataframes "dates" and "analysisArraydf" into one called "mergeFrame":
        mergeFrame <- merge(dates, analysisArraydf, by.x="date",by.y="Date")
        
        ggplot(mergeFrame, aes(x=weekday, y=weeknum, fill = NumberofCustomers)) + 
                geom_tile() +
                geom_text(aes(label=day)) + scale_fill_gradient(low="yellow", high="red") +
                labs(title = "Total Number of Customers", x="Week Day", y="Week Number")
        # scale_colour_gradient(limits=c(1, 10))           

        ggplot(mergeFrame, aes(x=weekday, y=weeknum, fill = NumberNewCustomers)) + 
                geom_tile() +
                geom_text(aes(label=day)) + scale_fill_gradient(low="yellow", high="red") # scale_colour_gradient(limits=c(1, 10))           
        