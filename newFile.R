dailyTOCalcbyService <- function(Service, CAList,  DateX)
        #This function calculates the average period in days between visits by CustomerNo
        #before date DateX in dataframe CAList...
{
        #Convert the CompletedDate field to numbers:
        
        uniqueDAYS <- as.POSIXct(CAList$CompletedDate, tz = "GMT", "%m/%d/%y")
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
