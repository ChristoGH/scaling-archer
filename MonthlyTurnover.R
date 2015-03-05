# With this script I will plot the monthly turnover figures
# Time Series Analysis
# Start by setting the right working directory:

        wd <- getwd()
        setwd("C:/rProgramming/ShopStats/Graphs")

#------------------------------------------------------------------------------------------------------
chShop <- odbcConnect("shopData")
        salesFrame <- sqlFetch(chShop, "salesFrame")
        productFrame <- sqlFetch(chShop, "productFrame")
        salesFrame$SalesDate<-as.POSIXct(as.Date(salesFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
        productFrame$SalesDate<-as.POSIXct(as.Date(productFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
        
odbcClose(chShop) 

        
uniqueSD <- unique(salesFrame$ServiceDescription)
DateFrom <- as.numeric(as.POSIXct("01/12/2014 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
DateTo <- as.numeric(as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))        
IndexToFrom <- (salesFrame$CompletedDate>=DateFrom) & (salesFrame$CompletedDate<DateTo)   
uniqueSI <- unique(salesFrame$ServiceItem)

 
        salesFrame$yMon <- as.yearmon(as.Date(salesFrame$SalesDate), "%YM%m")
        productFrame$yMon <- as.yearmon(as.Date(productFrame$SalesDate), "%YM%m")
        
        salesData <- aggregate(as.numeric(data.matrix(Value)) ~ (ServiceDescription+yMon), FUN = "sum", data = salesFrame)        
        productData <- aggregate(as.numeric(data.matrix(Value)) ~ (ProductType+yMon), FUN = "sum", data = productFrame)        
        #Let's set the header names:
        salesData <- setNames(salesData, c("ServiceDescription", "yMon", "AggregateValue"))
        productData <- setNames(productData, c("ProductDescription", "yMon", "AggregateValue"))
        
# Let us try and plot this lot:
# Set the file name
        datestr<-as.POSIXct(max(salesFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
        SalesMonthString <- format(datestr, "%B %Y")
        
        
        png(file=paste("Monthly Turnover by Service","_",SalesMonthString,".png",sep=""))                
        compplot <- ggplot(data = salesData, 
                           mapping = aes(x = factor(yMon), y = AggregateValue, fill = ServiceDescription)) +
                geom_bar(width=.7, stat="identity", alpha=0.7) +
                labs(title = paste("Monthly Turnover by SERVICE"), 
                     x=paste("Month",""), 
                     y=paste("Value in Rand"))+ 
                theme(
                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                        axis.text = element_text(size = rel(1), colour = "Black"),
                        plot.background = element_rect(fill = "transparent",colour = NA),
                        axis.text.x = element_text(angle = -90, vjust = 0.25, hjust=1)
                )  + scale_y_continuous(labels = comma)
        print(compplot)
        makeHeadnote(paste("Levingers Victory Park:", Sys.time()), color = "black")
        dev.off()
#-The following creates the graph for the product turnover-----------------------------------------------------------------------------------------------------        
        datestr <- as.POSIXct(max(productFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
        ProductMonthString <- format(datestr, "%B %Y")
        png(file=paste("Monthly Turnover by Product","_",ProductMonthString,".png",sep=""))                
        
        compplot <- ggplot(data = productData, 
                           mapping = aes(x = factor(yMon), y = AggregateValue, fill = ProductDescription)) +
                geom_bar(width=.7, stat="identity", alpha=0.7) +
                labs(title = paste("Monthly Turnover by PRODUCT"), 
                     x=paste("Month",""), 
                     y=paste("Value in Rand"))+ 
                theme(
                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                        axis.text = element_text(size = rel(1), colour = "Black"),
                        plot.background = element_rect(fill = "transparent",colour = NA),
                        axis.text.x = element_text(angle = -90, vjust = 0.25, hjust=1)
                )  + scale_y_continuous(labels = comma)
        print(compplot)
        makeHeadnote(paste("Levingers Victory Park:", Sys.time()), color = "black")
        dev.off()

#-Plot consolidated Stats-----------------------------------------------------------------------------------------------------        
        datestr <- as.POSIXct(max(productFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
        ProductMonthString <- format(datestr, "%B %Y")
        png(file=paste("Monthly Turnover by Product","_",ProductMonthString,".png",sep=""))                
        
        compplot <- ggplot(data = productData, 
                           mapping = aes(x = factor(yMon), y = AggregateValue, fill = ProductDescription)) +
                geom_bar(width=.7, stat="identity", alpha=0.7) +
                labs(title = paste("Monthly Turnover by PRODUCT"), 
                     x=paste("Month",""), 
                     y=paste("Value in Rand"))+ 
                theme(
                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                        axis.text = element_text(size = rel(1), colour = "Black"),
                        plot.background = element_rect(fill = "transparent",colour = NA),
                        axis.text.x = element_text(angle = -90, vjust = 0.25, hjust=1)
                )  + scale_y_continuous(labels = comma)
        print(compplot)
        makeHeadnote(paste("Levingers Victory Park:", Sys.time()), color = "black")
        dev.off()
        
setwd(wd)
        