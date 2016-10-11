ACOP <-
function(dat.d,vect.grps = 0){
  vect.grps=as.factor(as.character(vect.grps))

  num.grps<-length(table(vect.grps))
  topo.col<-rainbow(num.grps)
  names.grps<-levels(vect.grps)
  
  #### ACOP
  acop<-cmdscale(dat.d,k=nrow(as.matrix(dat.d))-5,eig=TRUE)
  axes.tot<-acop$eig
  inertia<-sum(acop$eig[acop$eig>0])
  percents<-round(as.numeric(100 * axes.tot/inertia),digits=0)

  #### Scores
  par()
  par(mfrow=c(1,2),pty="s")
  
  coord1<-acop$points[,c(1,2)]
  col.grps<-data.frame(vect.grps,coord1)
  plot(coord1,asp=1,cex=.1,
         xlab=paste("Axis 1 ","(",percents[1]," % variance explained)",sep=''), 
         ylab=paste("Axis 2 ","(",percents[2]," % variance explained)",sep=''),
         main="",type='n',bty='n')
  abline(h=0,lty=2,col='grey')
  abline(v=0,lty=2,col='grey')
      
  if(length(vect.grps) == nrow(as.matrix(dat.d))) {
    for(g in 1:length(names.grps)){
      text(x=coord1[col.grps[,1] == names.grps[g],1],y=coord1[col.grps[,1] == names.grps[g],2],labels=names.grps[g],col=topo.col[g],cex=.7)
      }     
  } else {
    points(coord1,pch=19,col="blue",cex=.5)
    }
    


  coord1<-acop$points[,c(3,4)]
  col.grps<-data.frame(vect.grps,coord1)
  plot(coord1,asp=1,cex=.1,
         xlab=paste("Axis 3 ","(",percents[3]," % variance explained)",sep=''), 
         ylab=paste("Axis 4 ","(",percents[4]," % variance explained)",sep=''),
         main="",type='n',bty='n')
  abline(h=0,lty=2,col='grey')
  abline(v=0,lty=2,col='grey')
          
  if(length(vect.grps) == nrow(as.matrix(dat.d))) {
    for(g in 1:length(names.grps)){
      text(x=coord1[col.grps[,1] == names.grps[g],1],y=coord1[col.grps[,1] == names.grps[g],2],labels=names.grps[g],col=topo.col[g],cex=.7)
      }     
  } else {
    points(coord1,pch=19,col="blue",cex=.5)
    }
    
  }
