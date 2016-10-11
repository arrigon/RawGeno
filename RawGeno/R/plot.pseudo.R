plot.pseudo <-
function(all.dat,peaks.info,path,scored=T,width=3000,height=6000,cex=.5,...){
  sizes=seq(min(as.numeric(all.dat[,2]))-1,max(as.numeric(all.dat[,2]))+1,by=.01)
  samples=levels(as.factor(all.dat[,1]))
  
  left=all.dat[,1]-.5
  rigth=all.dat[,1]+.5
  
  bin.low=aggregate(peaks.info[,1],by=list(peaks.info[,4]),min)
  bin.up=aggregate(peaks.info[,1],by=list(peaks.info[,4]),max)
  bin.pos=(bin.low[,2]+bin.up[,2])/2
  
  ### Plot Bins
  pdf(file = paste(path,"pseudoGel.pdf",sep=.Platform$file.sep),bg="white",height=82.7,width=118.1)
  plot(x=all.dat[,1],y=all.dat[,2],type='n',xlim=c(0,max(all.dat[,1])+1),xlab='')

  rect(1-.5,bin.low[,2],max(all.dat[,1])+.5,bin.up[,2], col=grey(0.9),border=NA)
  segments(x0=left,y0=all.dat[,2],x1=rigth,y1=all.dat[,2],col='black',lwd=.02)
  if(scored==T) text(x=peaks.info[,2],y=peaks.info[,1],col='black',labels=peaks.info[,2],cex=.5,pos=1,offset=.2)
  
  text(x=1-.6,y=bin.pos,labels=as.character(bin.pos),pos=2,cex=.6)
  text(x=max(all.dat[,1])+.5,y=bin.pos,labels=as.character(bin.pos),pos=4,cex=.6)  
    
  dev.off()
  }
