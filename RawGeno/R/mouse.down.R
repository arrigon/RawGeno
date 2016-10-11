mouse.down <-
function (x) {                                                                      
    if (tclvalue(rbValue) == "Edit") {                                 
        modified <<- 1 
        autoscoring <<- 0
        x <<- x                                                        
        shift <<- (mean(c(ur, ul)) - initf)                            
        deltaU <<- abs(ur - ul)                                        
        PltoU <<- deltaU/initspan                                      
        zmu <<- zms * PltoU                                            
        centerscreen <<- mean(c(ur, ul)) + (focs - initf)              
        urx <<- centerscreen + zmu                                     
        ulx <<- centerscreen - zmu                                     
        tx <<- (as.numeric(x) - 1)/iw                                  
        txx <<- tx * urx + (1 - tx) * ulx                              
        iw <<- iw                                                      
        ms <<- as.numeric(x)                                           
        diffs = apply(xx[, 1:2], 1, diff)                              
        mins = apply(xx[, 1:2], 1, min)                                
        maxs = apply(xx[, 1:2], 1, max)                                
        lims = cbind(mins + 0.15 * diffs, maxs - 0.15 * diffs)         
        test = txx >= lims[, 1] & txx <= lims[, 2]                     
        test = which(test == T)                                        
        inpol = F                                                      
        if (length(test) > 0) {                                        
            ci <<- test                                                
            inpol <<- T                                                
        }
        else {
            ci <<- which.min(abs(txx - xx[, 1:2]))
            inpol <<- F
        }
        md <<- TRUE
        mouse.move(x)
    }
}
