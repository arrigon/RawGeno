ORDIN.INDIV <-
function(matinfo=0,
                   column=-9,
                   columnDiagno=0,
                   states=0,
                   diagno='NormFreq',
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
  
  if(diagno=='External'){
    chck=extnr
    assign('chck',chck,pos=1)
    chck=chck
    }
  
  if(diagno=='NumBins'){
    chck=round(as.numeric(nbbin),2)
  }
  
  if(diagno=='NormFreq'){
    chck=round(as.numeric(binFR),2)
  }
  
  if(diagno=='HeightVar'){
    chck=round(as.numeric(h.var),2)
  }
  
  if(diagno=='HeightMean'){
    chck=round(as.numeric(h.mean),2)
  }

  ### Visualize diagno 
  bin.d = vegdist(data.bin,'jaccard')    
  ACOP(bin.d,chck)
  }
