SHOWPLATES <-
function(vard='NumBins',p=1,txt='Tag',pathpdf=0,logv=F){
  b.01=t(data.binary$data.binary)
  b.height=t(data.binary$data.height)
  b.size=t(data.binary$data.size)

  ### Prepare diagnostics
  nbbin=rowSums(b.01)
  binFreq=colMeans(b.01,na.rm=T)
  binFR=t(t(b.01)*binFreq)
  binFR[binFR==0]=NA
  binFR=rowMeans(binFR,na.rm=T)

  data.test=b.height
  data.test[data.test==0]=NA
  h.var=apply(data.test,1,sd,na.rm=T)
  h.mean=rowMeans(data.test,na.rm=T)
  h.sum=rowSums(data.test,na.rm=T)

  data.diags=data.frame(NumBins=nbbin,NormFreq=binFR,HeightVar=h.var,HeightMean=h.mean)

  ### Join binary table and Info table
  popsA=row.names(data.diags)
  popsB=matinfo$Tag
  inters.A=data.diags[match(intersect(popsA,popsB),popsA),]
  inters.B=matinfo[match(intersect(popsA,popsB),popsB),]
  x=cbind(inters.B,inters.A)
  
  ### convert matinfo$pos into 1:96 numbering (if necessary)
  AH=rep(c("A","B","C","D","E","F","G","H"),12)
  nm=rep(1:12,each=8)
  ser=paste(AH,nm,sep='')

  AH=rep(c("A","B","C","D","E","F","G","H"),12)
  nm=rep(1:12,each=8)
  nm=paste(rep(0,96),nm,sep='')
  nm=substr(nm,nchar(nm)-1,nchar(nm))
  ser1=paste(AH,nm,sep='')
  trslte=data.frame(ser,ser1,1:96)
  
  test=NULL
  for(i in 1:3){
    nt=sum(match(x$Pos,trslte[,i]))
    test=c(test,nt)
    }
  if(sum(!is.na(test))==1){
    OKcol=trslte[,which(!is.na(test))]
    x$Pos=trslte[match(x$Pos,OKcol),3]
    } else {
    stop("Please check your numbering of PCR well plates, there seems to be an error...\n")
    }
     
  x1=x[x$Plate==p,]
  targ=match(vard,colnames(x))
  
  x1=x1[order(x1$Pos),]
  displayed=x1[,targ]
  maxmes=max(x[,targ],na.rm=T)

  link=cbind(as.numeric(as.character(x1$Pos)),displayed)
  link=rbind(link,cbind(1:96,NA))
  link=link[order(link[,1]),]
  mes1=as.vector(tapply(link[,2],as.factor(link[,1]),sum,na.rm=T))
  mes=rev(maxmes-mes1) 
  
  if(logv) mes1=log(mes1+1)
  if(logv) mes=log(mes+1)
   
  info.n=t(matrix(mes,8,12))
  info.n=info.n[,order(8:1)]
  
  ### Retrieve Tag
  subm=data.frame(Tag=as.character(x1$Tag),Pos=x1$Pos)
  coords=data.frame(x=rep(8:1,12),y=rep(12:1,each=8),mes1)
  Tags=rep('empty',96)
  Tags[subm$Pos]=as.character(subm$Tag)
  coords=data.frame(coords,Tag=Tags)

  ## plot
    plotfun=function(...){
    nf=layout(matrix(c(2,4,1,3),2,2,byrow=TRUE), c(3,1.5), c(1.5,3), TRUE)
    par(mar=c(3,3,1,1))
    plot.gel(info.n,nrgcols=50,main='')
    axis(1,1:8,LETTERS[1:8],cex.axis=.55)
    axis(2,1:12,1:12,cex.axis=.55,las=1)
    axis(3,1:8,LETTERS[1:8],cex.axis=.55)
    axis(4,1:12,1:12,cex.axis=.55,las=1)
    if(txt=='Stats') text(x=coords$x,y=coords$y,labels=rev(as.character(round(coords$mes1,2))),cex=.7)
    if(txt=='Tag') text(x=coords$x,y=coords$y,labels=rev(as.character(coords$Tag)),cex=.5,srt=45)
    
    par(mar=c(2,2,3,0))
    axevx=(9-coords$x)
    boxplot(coords$mes1~axevx,axes=F)
    par(mar=c(2,2,0,0))
    axevy=(13-coords$y)
    boxplot(coords$mes1~axevy,horizontal=T,axes=F)
    par(mar=c(3,3,1,0))
    plot(0,xlim=c(0,10),ylim=c(0,10),axes=F,type='n')
    text(x=1,y=9,labels=paste('Plate',p),cex=1.5,pos=4,font=2)
    text(x=2,y=7,labels=paste('Info :',vard),cex=1,pos=4)
    text(x=2,y=5,labels=paste('Labels :',txt),cex=1,pos=4)
    text(x=2,y=3,labels=paste('Log :',logv),cex=1,pos=4)
    }
  if(pathpdf==0){
    if (Sys.info()[1] == "Windows") windows() 
    plotfun()
    } else {
    file=paste(pathpdf,.Platform$file.sep,'PCRmap_P',p,'_',vard,'.pdf',sep='')
    pdf(file)
    plotfun()
    cat('Done! Pdf files were saved in', getwd(),'\n')
    dev.off()
    }  
  }
