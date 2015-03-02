
# Here are a few calculations used for ShopStats
# meanDaysCalc <- function(CustomerNo, CAList,  DateX)

meanDaysCalc <- function(CustomerNo, CAList,  DateX)
        #This function calculates the average period in days between visits by CustomerNo
        #before date DateX in dataframe CAList...
{
        #Convert the CompletedDate field to numbers:
        nDL <- array(as.numeric(as.POSIXct(CAList$CompletedDate, tz = "", "%d/%m/%Y %H:%M"))) 
        #Find all instances in CompletedDate that occurred BEFORE  DateX:                      
        FDTI <- array(nDL <= DateX)
        #Find all the visits by CustomerNo
        FCI <- array(CAList$Customer)==CustomerNo
        if (sum(FDTI * FCI)<2)
        {mPeriod = NA}
        else
        {
                #Calculate all the visits by CustomerNo before DateX
                FI <- rfind(FDTI * FCI)
                ##CAListA<-CAList[rfind(FilterIndex),]        
                ##sd<-as.POSIXct(CAList$CompletedDate, tz = "", "%d/%m/%Y %H:%M")
                FNDL <- nDL[FI]
                # Calculate the Period of vists in SECONDS:
                vPeriod <- FNDL[2:length(FNDL)] - FNDL[1:(length(FNDL)-1)]
                # And finally the MEAN period in DAYS:
                mPeriod <- mean(vPeriod[rfind(vPeriod>0)])/(60*60*24)
        }
        ##eomDate<-timeLastDayInMonth(sd)
        ##monthList<-unique(eomDate)
        ##X <- (monthList) #array(c(1,2,3,45,0,7,0))
        ## mDC<-apply(X, 1, rfind, na.rm = TRUE)
return(mPeriod)}

turnOverCalc <- function(CAList,  yearMon)
        #This function calculates the turnover  days between visits by CustomerNo
        #before date DateX in dataframe CAList...
{
        #Convert the CompletedDate field to numbers:
        nDL <- array(as.numeric(as.POSIXct(CAList$CompletedDate, tz = "", "%d/%m/%Y %H:%M"))) 
        #Find all instances in CompletedDate that occurred BEFORE  DateX:                      
        FDTI <- array(nDL <= DateX)
        #Find all the visits by CustomerNo
        FCI <- array(CAList$Customer)==CustomerNo
        if (sum(FDTI * FCI)<2)
        {mPeriod = NA}
        else
        {
                #Calculate all the visits by CustomerNo before DateX
                FI <- rfind(FDTI * FCI)
                ##CAListA<-CAList[rfind(FilterIndex),]        
                ##sd<-as.POSIXct(CAList$CompletedDate, tz = "", "%d/%m/%Y %H:%M")
                FNDL <- nDL[FI]
                # Calculate the Period of vists in SECONDS:
                vPeriod <- FNDL[2:length(FNDL)] - FNDL[1:(length(FNDL)-1)]
                # And finally the MEAN period in DAYS:
                mPeriod <- mean(vPeriod[rfind(vPeriod>0)])/(60*60*24)
        }
        ##eomDate<-timeLastDayInMonth(sd)
        ##monthList<-unique(eomDate)
        ##X <- (monthList) #array(c(1,2,3,45,0,7,0))
        ## mDC<-apply(X, 1, rfind, na.rm = TRUE)
        return(mPeriod)}


MDCustomer <- function(DateX, CAList)
{
        # Create an ARRAY of UNIQUE customers, like so:
                CustomerList <- array(unique(CAList$Customer))
                
        # Troll through the list of customers and for EACH customer calculate the mean period in days between visits,
        # but ONLY for those visists that happened BEFORE DateX:
                FrequencyList<-lapply(CustomerList, meanDaysCalc, CAList, DateX)
        
        #Return FrequencyList to an UNLISTED state
                FrequencyUnList <- unlist(FrequencyList)
        
        # Strip out NAs
                FrequencyUnList <- FrequencyUnList[(!is.nan(FrequencyUnList))]
        
        # Strip out NaNs
                FrequencyUnList <- FrequencyUnList[(!is.na(FrequencyUnList))]
        
        # ow draw a HISTOGRAM of the customer return frequency
                hist(FrequencyUnList,20, main = "Histogram of Customer Return Frequency", xlab = "Days")
        return(FrequencyUnList)
}

NewCustomerbyDay <- function(DateX, CAList)
{
        # Create an ARRAY of UNIQUE customers, like so:
        CustomerList <- array(unique(CAList$Customer))
        
        # Troll through the list of customers and for EACH customer calculate the mean period in days between visits,
        # but ONLY for those visists that happened BEFORE DateX:
        FrequencyList<-lapply(CustomerList, meanDaysCalc, CAList, DateX)
        
        #Return FrequencyList to an UNLISTED state
        FrequencyUnList <- unlist(FrequencyList)
        
        # Strip out NAs
        FrequencyUnList <- FrequencyUnList[(!is.nan(FrequencyUnList))]
        
        # Strip out NaNs
        FrequencyUnList <- FrequencyUnList[(!is.na(FrequencyUnList))]
        
        # ow draw a HISTOGRAM of the customer return frequency
        hist(FrequencyUnList,20, main = "Histogram of Customer Return Frequency", xlab = "Days")
        
}