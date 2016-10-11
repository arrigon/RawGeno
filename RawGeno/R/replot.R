replot <-
function (...){                                                                                                                                                                                  
    focs <<- as.numeric(tclvalue(focus))                                                                                                                                           
    zms <<- as.numeric(tclvalue(zoom))                                                                                                                                             
    lft <<- focs - zms                                                                                                                                                             
    rht <<- focs + zms                                                                                                                                                             
    xx.pl <<- rbind(xx[xx[, 1] >= lft & xx[, 2] <= rht, ])
#     meta.pl <<- rbind(meta[xx[, 1] >= lft & xx[, 2] <= rht, ])
    numbs.pl = numbs[xx[, 1] >= lft & xx[, 2] <= rht]                                                                                                                              
    hom.pl = round(hom[xx[, 1] >= lft & xx[, 2] <= rht], 2)                                                                                                                        
    nbin = nrow(xx.pl)
    plot(0, 0, type = "n", xlab = "Size (bp)",                                                                                                                   
        ylab = "Fluorescence (rfu)", xlim = c(lft, rht), ylim = c(0,                                                                                                               
            as.numeric(tclvalue(yzm))))                                                                                                                                            
    mtext(paste("Bin size:\nOccurrences:\nHomoplasy:"), side = 3,                                                                                                                  
        line = 1, at = lft, adj = 1, cex = 0.8)                                                                                                                                    
    mtext(paste("Modify bins: click-and-drag green areas \nRemove or Add bins: right click"),                                                                                      
        side = 1, at = lft, adj = 0, cex = 0.8, line = 4)                                                                                                                          
    if (!is.null(nbin)) {                                                                                                                                                          
        if (nbin > 1) {                                                                                                                                                            
            for (i in 1:nrow(xx.pl)) {                                                                                                                                             
                polygon(c(xx.pl[i, 1], xx.pl[i, 2], xx.pl[i,                                                                                                                       
                  2], xx.pl[i, 1]), c(yy[2], yy[2], yy[1], yy[1]),                                                                                                                 
                  col = "#009900")                                                                                                                                                 
            }                                                                                                                                                                      
            mtext(paste(round(rowMeans(xx.pl[,1:2]), 2), "bp", "\n",                                                                                                                     
                numbs.pl, "ind.", "\n", "HR", hom.pl), side = 3,                                                                                                                   
                line = 1, at = xx.pl[, 1] + apply(xx.pl[,1:2], 1, diff)/2,                                                                                                               
                cex = 0.8)                                                                                                                                                         
        }                                                                                                                                                                          
        if (nbin == 1) {                                                                                                                                                           
            polygon(c(xx.pl[1], xx.pl[2], xx.pl[2], xx.pl[1]),                                                                                                                     
                c(yy[2], yy[2], yy[1], yy[1]), col = "#009900")                                                                                                                    
            mtext(paste(round(mean(xx.pl[1:2]), 2), "bp", "\n", numbs.pl,                                                                                                               
                "ind.", "\n", "HR", hom.pl), side = 3, line = 1,                                                                                                                   
                at = xx.pl[1,1] + diff(xx.pl[1,1:2])/2, cex = 0.8)                                                                                                                          
        }                                                                                                                                                                          
    }                                                                                                                                                                              
    for (i in 2:ncol(polyy)) {
        lines(polyy[, 1], polyy[, i])
    }
    if (first) {
        first <<- FALSE
        tmp = cnvrt.coords(c(0, 1), c(0, 1), input = "dev")$usr
        ul <<- tmp$x[1]
        ur <<- tmp$x[2]
    }
    assign("out", yy, env = .GlobalEnv)
}
