VISUALIZE <-
function(matinfo=0,
                   column=-9,
                   columnDiagno=0,
                   states=0,
                   diagno='NormFreq',
                   revise=F,
                   output=F,
                   mergedT=T){
                   
  if(mergedT==T){
    data=mergedTable
    b.height=mergedTable
    b.size=mergedTable
  } else {
    data=t(data.binary$data.binary)
    b.height=t(data.binary$data.height)
    b.size=t(data.binary$data.size)
  }
  
  if(matinfo==0){
    matinfo=data.frame(Tag=rownames(data),x=rep(0,nrow(data)))
    column='Tag'
    states=levels(matinfo$Tag)
    }
  
  ### Join binary table and Info table
  popsA=row.names(data)
  popsB=matinfo$Tag
  inters.A1=data[match(intersect(popsA,popsB),popsA),]
  inters.A=inters.A1[,colSums(inters.A1)>0]
  inters.B=matinfo[match(intersect(popsA,popsB),popsB),]
  x=cbind(inters.B,inters.A)
  
  b.height=b.height[match(intersect(popsA,popsB),popsA),colSums(inters.A1)>0]
  b.size=b.size[match(intersect(popsA,popsB),popsA),colSums(inters.A1)>0] 
  
  columnToUse=inters.B[,match(column,colnames(matinfo))]

  ### Select individuals to display
  data.show=x[is.na(match(columnToUse,states))==F,]
  assign("data.bin",data.show[,(ncol(inters.B)+1):ncol(data.show)],pos=1)
  assign("data.show",data.show,pos=1)
  
  ### Prepare diagnostics
  if(columnDiagno==0){
    extnr=rep(1,nrow(data.show))
    } else {
    extnr=data.show[,match(columnDiagno,colnames(data.show))]
    }
  nbbin=rowSums(data.bin)
  binFreq=colMeans(data.bin,na.rm=T)
  binFR=t(t(data.bin)*binFreq)
  binFR[binFR==0]=NA
  binFR=rowMeans(binFR,na.rm=T)
    
  data.test=b.height
  data.test[data.test==0]=NA
  popsA=rownames(data.test)
  popsB=as.character(matinfo$Tag)
  A=data.test[match(intersect(popsA,popsB),popsA),]
  B=matinfo[match(intersect(popsA,popsB),popsB),]
  xx=cbind(B,A)
  show=xx[is.na(match(columnToUse,states))==F,]
  data.test=show[,(ncol(B)+1):ncol(show)]
  sizes=as.numeric(colnames(A))
  h.var=apply(data.test,1,sd,na.rm=T)
  h.mean=rowMeans(data.test,na.rm=T)
    
  if(output==T){
    sample.diags=data.frame(NumbBins=nbbin,R=binFR,BinHeightVar=h.var,BinHeightMean=h.mean)
    write.table(sample.diags,'SamplesDiagnosticValues.txt',sep='\t')
    cat('SamplesDiagnosticValues.txt was produced in',)
    }
  
  if(diagno=='External'){
    chck=extnr
    assign('chck',chck,pos=1)
    chck=as.numeric(chck)
    }
  
  if(diagno=='NumBins'){
    chck=nbbin
  }
  
  if(diagno=='NormFreq'){
    chck=binFR
  }
  
  if(diagno=='HeightVar'){
    chck=h.var
  }
  
  if(diagno=='HeightMean'){
    chck=h.mean
  }

  ### Visualize diagno
  rc=rev(heat.colors(30))
  if(diagno=='External'){
    rc=rainbow(max(chck))
    rc=data.frame(no=1:length(rc),rc)
    if(max(chck)<2){
      splt=2
      } else {
      splt=max(chck)
      } 
    chck=data.frame(num=1:length(chck),no=as.numeric(cut(chck,splt)))
    } else {
    rc=data.frame(no=1:length(rc),rc)
    chck=data.frame(num=1:length(chck),no=as.numeric(cut(chck,30)))
    }
  
  chck=merge(chck,rc,by='no')
  rc=as.character(chck[order(chck[,2]),3])

  par()
    res=heatmap(as.matrix(data.bin[,ncol(data.bin):1]),
          cexRow=.8,
          labRow=as.character(data.show$Tag),
          Colv=NA,
          labCol=F,
          col=c("black","red"),
          add.expr=rowSums(data.bin),
          scale="none",
          RowSideColors=rc)$rowInd
             
  if(revise==T){        
    res=rev(res)         
    windows(rescale='R',width = 30, height = 25,bg='black')        
    plot.gel(as.matrix(data.bin[res,]),
             rlabels=as.character(data.show[res,]$Tag),main='LABEL SAMPLES',        
             rcols=rc[res])
    torem=round(locator(n=nrow(data.bin),type='p',col='green')$y,0)
    write.table(torem,paste(states[1],'markedsamples.txt',sep=''),sep='\t')
    }  
  }
