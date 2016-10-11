vekemans.test <-
function(data.binary=0,bands.pos=0,bands.freq=0,plot='F'){
  if(sum(data.binary)!=0){
    bands.pos=as.numeric(row.names(data.binary))
    bands.freq=apply(data.binary,1,mean)
    }
    
  test=cor.test(bands.pos,bands.freq)
  correlation=test$estimate
  p.value=test$p.value
  if(plot=='T'){
    plot(x=bands.pos,y=bands.freq,col='grey',xlab='Bin size (bp)'
        ,ylab='Bin Frequency',main='Size Homoplasy'
        ,sub=paste('R^2 =',round(as.numeric(correlation),3),'P-value=',round(as.numeric(p.value),3)))
    abline(lm(bands.freq~bands.pos),col='blue')
    }
  data.frame(correlation,p.value)
  }
