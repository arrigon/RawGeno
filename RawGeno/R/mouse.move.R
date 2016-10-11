mouse.move <-
function(x) {
  if (md) {
    # get new position of cursor
    tx <<- (as.numeric(x) - 1)/iw
    ccx <<- tx * urx + (1 - tx) * ulx
              
    # Modify either position or bin limits
    if(ci>0 & inpol==T){
      xx[ci,1:2] = xx[ci,1:2]-(mean(xx[ci,1:2])-ccx)
      } else {
      xx[ci] = ccx              
      }  
      yy <- c(-2,2)
      
    ## orient xx
    xx[,1]=apply(xx[,1:2],1,min)
    xx[,2]=apply(xx[,1:2],1,max)
    xx <<- xx    
      
    # Recompute presences/bin (update numbs)
    newlims=xx[which(rownames(xx)==names(ci)),]
    numbs[which(rownames(xx)==names(ci))]=nlevels(as.factor(peaks.info[peaks.info[,1]>=newlims[1]&peaks.info[,1]<=newlims[2],2]))
    numbs <<- numbs
    hom[which(rownames(xx)==names(ci))]=mean(table((peaks.info[peaks.info[,1]>=newlims[1]&peaks.info[,1]<=newlims[2],2])))
    hom <<- hom
      
    tkrreplot(img)
    }   
  }
