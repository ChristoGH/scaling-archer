# This invokes the ODBC package to call SQL

DateFrom <- as.numeric(as.POSIXct("01/01/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))        #The unique ServiceItem values are found so:
DateTo <- as.numeric(as.POSIXct("01/02/2015 00:00", tz = "", "%d/%m/%Y %H:%M"))        
datestr<-as.POSIXct("01/01/2015 00:00", tz = "", "%d/%m/%Y %H:%M")
MonthString <- format(datestr, "%B %Y")

## This script connects to the latest iLaundry db on this computer
chShop <- odbcConnect("shopData")
        productFrame <- sqlFetch(chShop, "productFrame")
#         salesFrame$SalesDate<-as.POSIXct(as.Date(salesFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")
        productFrame$SalesDate<-as.POSIXct(as.Date(productFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")

odbcClose(chShop) 

replaceBattery=function(i,eFTable){
        x<-eFTable[i,]
       x$ProductType<-as.character(ifelse(as.character(x$Product)=="Cufflink","Cufflink",as.character(x$ProductType)))
return(x)
}


#sqlReturnTable <- sqlQuery(ch, paste(SqlString))  
productFrame$SalesDate<-as.POSIXct(as.Date(productFrame$SalesDate), tz = "", "%d/%m/%Y %H:%M")

IndexToFrom <- (productFrame$SalesDate<DateTo)&(productFrame$SalesDate>=DateFrom)
newMonthTable <- productFrame[IndexToFrom,]


newMonthTable<-lapply(1:length(newMonthTable[,1]),FUN=replaceBattery,newMonthTable)
newMonthTable <- ldply(newMonthTable, data.frame)


ProductTable <- aggregate(Value ~ Product, FUN = sum, data=newMonthTable) 
ProductTypeTable <- aggregate(Value ~ ProductType, FUN = sum, data=newMonthTable) 
ProductTable <- ProductTable[order(-ProductTable[,2]),]

#ProductTypeTable <- transform(ProductTypeTable, ProductType=reorder(ProductType, Value) ) 
ProductTypeTable <- arrange(ProductTypeTable, -Value)

# aggSITable <- aggSITable[order(-aggSITable[,2]),]
# ProductTypeTable <- aggregate(Value ~ ProductType, FUN = sum, data=newMonthTable) 

PT<-ProductTable
PT[,3]<-cumsum(ProductTable[,2])
PT[,4]<-PT[,3]/sum(ProductTable[,2])
exclTableVector <- !(PT[,4]>0.80)
ProductTable80 <- ProductTable[exclTableVector,]
write.csv(file="C:/Users/Admin/Creative Cloud Files/ShopStats/ProductTable80.csv", x=ProductTable80)
odbcClose(ch)       
#with(newMonthTable, newMonthTable$Product<-ifelse(ProductType=="Cufflink","Cufflink",Product))
#The following line of code replaces Battery as ProductType when Product= "Cufflink"
newMonthTable<-lapply(1:length(newMonthTable[,1]),FUN=replaceBattery,newMonthTable)

ProductTypeTable$NormalValue <- ProductTypeTable$Value/max(ProductTypeTable$Value)
ProductTypeTable <- arrange(ProductTypeTable, -Value)
ProductPLOTfunction(TableTF = ProductTypeTable, 
                                FileDescriptionString = "Levingers Victory Park - Counter - ", 
                                DepartmentString = "Counter Sales",
                                MonthString = MonthString, 
                                FillColour = "green")

