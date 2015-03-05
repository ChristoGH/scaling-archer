# Run this script FIRST.  It invokes some libraries and define some functions
# Place newer function FIRST and more generic older TESTED functions
# further down below.

library(scales)
library(dplyr)
library(timeDate)
library(zoo)
library(date)
library(chron)
library(reshape2)
        # This is for functions such as melt, dcast and reshape.
library(ggplot2)           
library(gridExtra)
library(RODBC)
library(plyr)


ServicePLOTfunction <- function(TableTF, 
                                FileDescriptionString, 
                                DepartmentString,
                                MonthString, 
                                FillColour){
#         usage:
#                 ServicePLOTfunction <- function(TableTF = aggCobblingSTTableTF, 
#                                                 FileDescriptionString = "Levingers Victory Park - Cobbling - ", 
#                                                 DepartmentString = "Cobbling",
#                                                 MonthString = MonthString, 
#                                                 FillColour = "navy")
                        
        png(file=paste(FileDescriptionString, MonthString,".png",sep=""))                
        compplot <- ggplot(data = TableTF, 
                    mapping = aes(x = ServiceItem, y = Value)) +
                geom_bar(colour="black", fill = FillColour, width=.7, stat="identity", alpha=0.7) +
                labs(title = paste(DepartmentString, "turnover by Item"), 
                     x=paste("Service Item \n",""), 
                     y=paste("Value in Rand"))+ 
                theme(
                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                        axis.text = element_text(size = rel(1), colour = "Black"),
                        plot.background = element_rect(fill = "transparent",colour = NA)
                ) +
                coord_flip() + scale_y_continuous(labels = comma)
                print(compplot)
                makeHeadnote(paste("Shop statistics for",MonthString), color = "black")
        dev.off()
}

ProductPLOTfunction <- function(TableTF, 
                                FileDescriptionString, 
                                DepartmentString,
                                MonthString, 
                                FillColour){
        #         usage:
        #                 ProductPLOTfunction <- function(TableTF = ProductTypeTable, 
        #                                                 FileDescriptionString = "Levingers Victory Park - Counter - ", 
        #                                                 DepartmentString = "Counter Sales",
        #                                                 MonthString = MonthString, 
        #                                                 FillColour = "green")
        
        png(file=paste(FileDescriptionString, MonthString,".png",sep=""))                
        compplot <- ggplot(data = TableTF, 
                           mapping = aes(x = ProductType, y = NormalValue)) +
                geom_bar(colour="black", fill = FillColour, width=.7, stat="identity", alpha=0.7) +
                labs(title = paste(DepartmentString, "turnover by Item"), 
                     x=paste("Product Group \n",""), 
                     y=paste("Value in Rand"))+ 
                theme(
                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                        axis.text = element_text(size = rel(1), colour = "Black"),
                        plot.background = element_rect(fill = "transparent",colour = NA)
                ) +
                coord_flip() + scale_y_continuous(labels = comma)
        print(compplot)
        makeHeadnote(paste("Shop statistics for",MonthString), color = "black")
        dev.off()
}

makeFootnote <- function(footnoteText=
                                 format(Sys.time(), "%d %b %Y"),
                         size= .5, color= grey(.5))
{
        require(grid)
        pushViewport(viewport())
        grid.text(label= footnoteText ,
                  x = unit(1,"npc") - unit(2, "mm"),
                  y= unit(2, "mm"),
                  just=c("right", "bottom"),
                  gp=gpar(cex= size, col=color))
        popViewport()
}


makeHeadnote <- function(headnoteText=
                                 format(Sys.time(), "%d %b %Y"),
                         size= .5, color= grey(.5))
{
        require(grid)
        pushViewport(viewport())
        grid.text(label= headnoteText ,
                  x = unit(1,"npc") - unit(2, "mm"),
                  y= unit(1,"npc") - unit(2, "mm"),
                  just=c("right", "top"),
                  gp=gpar(cex= size, col=color))
        popViewport()
}



replaceItems=function(i,Table){
        x<-Table[i,]
        x$ServiceItem<-as.character(ifelse(grepl("Zipper",x$ServiceItem),"Zipper",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Mending",x$ServiceItem),"Mending",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Glueing",x$ServiceItem),"Glueing",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Patching",x$ServiceItem),"Patching",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Stitching",x$ServiceItem),"Stitching",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Denim",x$ServiceItem),"Denim",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Pants",x$ServiceItem),"Pants",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Heels",x$ServiceItem),"Heels",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Soles",x$ServiceItem),"Soles",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Blouse",x$ServiceItem),"Blouse",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Dress",x$ServiceItem),"Dress",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Elastic",x$ServiceItem),"Elastic",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Shirt",x$ServiceItem),"Shirt",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Jacket",x$ServiceItem),"Jacket",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Coat",x$ServiceItem),"Coat",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Suit",x$ServiceItem),"Suit",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Skirt",x$ServiceItem),"Skirt",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Sew",x$ServiceItem),"Sew",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Stitching",x$ServiceItem),"Stitch",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("WaterProofing",x$ServiceItem),"Waterproofing",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Buckle",x$ServiceItem),"Buckle",as.character(x$ServiceItem)))
        x$ServiceItem<-as.character(ifelse(grepl("Clean",x$ServiceItem),"Cleaning",as.character(x$ServiceItem)))
        
        return(x)
}
