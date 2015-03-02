

library(ggplot2); 
library(gridExtra)
my_hist<-ggplot(aggSIWeekdf1, aes(clarity, fill=cut)) + geom_bar() 
my_hist

my_table<- tableGrob(head(aggSIWeekdf1)[,1:3], 
                     gpar.coretext =gpar(fontsize=8), gpar.coltext=gpar(fontsize=8),  
                     gpar.rowtext=gpar(fontsize=8)) 

g_legend<-function(a.gplot){ 
        tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
        legend <- tmp$grobs[[leg]] 
        return(legend)} 

legend <- g_legend(my_hist) 
grid.draw(legend) 
