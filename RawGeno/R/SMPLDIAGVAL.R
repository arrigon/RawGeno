SMPLDIAGVAL <-
function(path=getwd()){
  if(exists("mergedTable")){
    data.bin=mergedTable
    } else {
    data.bin=t(data.binary$data.binary)
    }
  
  ### Prepare diagnostics
  nbbin=rowSums(data.bin)
  binFreq=colMeans(data.bin,na.rm=T)
  binFR=t(t(data.bin)*binFreq)
  binFR[binFR==0]=NA
  binFR=rowMeans(binFR,na.rm=T)

  if(exists("mergedTable")){
    data.test=mergedTable
    } else {
    data.test=t(data.binary$data.height)
    }
  data.test[data.test==0]=NA
  h.var=apply(data.test,1,sd,na.rm=T)
  h.mean=rowMeans(data.test,na.rm=T)

  sample.diags=data.frame(NumbPeaks=nbbin,R=binFR,BinHeightMean=h.mean,BinHeightSD=h.var)
  write.table(sample.diags,paste(path,.Platform$file.sep,'SamplesDiagnosticValues.txt',sep=''),sep='\t')
  cat('Done!\n')
  }
