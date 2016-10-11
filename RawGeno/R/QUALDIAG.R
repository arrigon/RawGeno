QUALDIAG <-
function(...){
  data.bin=data.binary$data.binary
  data.size=data.binary$data.size
  data.height=data.binary$data.size
  data.hom=data.binary$data.hom
  
  data.size[data.size==0]=NA
  data.height[data.size==0]=NA
  data.hom[data.size==0]=NA
  
  bin.stats=rowSums(data.bin)
  bin.stats=data.frame('Aggregates'=names(bin.stats),'NativeBands'=as.numeric(bin.stats))
  bin.mob=smooth.spline(y=bin.stats[,2],x=1:length(bin.stats[,2]),spar=.1)$y
    
  bin.low=apply(data.size,1,min,na.rm=T)
  bin.up=apply(data.size,1,max,na.rm=T)
  bin.pos=(bin.low+bin.up)/2
  
  bands.stats=bin.up-bin.low
  bands.mob=smooth.spline(y=bands.stats,x=1:length(bin.pos),spar=.2)$y
  
  val.height=data.binary$data.height
  val.sizes=data.binary$data.size
  val.height2=val.height[val.height!=0]
  val.height=val.height2
  
  val.sizes=val.sizes[val.sizes!=0]
  sub=sample(c(1:length(val.height)),length(val.height)/5)
  sm=smooth.spline(val.sizes[sub],val.height[sub],spar=.9)

  ## Homoplasy measures
  data.hom[is.na(data.hom)]=0
  hom.vect=as.vector(data.hom)
  hom.vect=hom.vect[hom.vect!=0]
  hom.bands=apply(data.hom,1,max)
  hom.mob=smooth.spline(y=hom.bands,x=1:length(hom.bands))$y
    
  par()
  par(mfrow=c(2,4),pty='s')
  ## Up
  plot(x=bin.pos,y=bands.stats,type='h',xlab='Bin Sizes (bp)', ylab='Bin Widths (bp)',main='Aggregation pattern',col='grey')
  lines(x=bin.pos,y=bands.mob,col='blue')    
   
  plot(x=bin.pos,hom.bands,type='h',
  ,main='Technical Homoplasy'
  ,xlab='Bin Sizes (bp)', 
  ,ylab='Peaks from same individual / Bin',col='grey')
  lines(x=bin.pos,y=hom.mob,col='red')
   
  vekemans.test=vekemans.test(data.bin,plot='T') 

  plot(x=val.sizes[sub],y=val.height[sub], 
     xlab='Bin Sizes (bp)', 
     ylab='Peaks height (standardized rfu)',
     main='Peaks intensity',col='grey',
     sub='(only 1/5 of the whole datast is shown)')     
  lines(x=sm$x,y=sm$y,col='blue')

  # low
  plot(density(bands.stats),
       col='blue',
       main='Bin Widths',
       xlab='Bin Widths (bp)',
       sub=paste('Mean(Widths) =',
       round(mean(bands.stats),2),'bp'),
       xlim=c(0,max(bands.stats)))
  abline(v=mean(bands.stats),col='green')
  
  plot(density(hom.vect),col='black',main='Technical Homoplasy'
      ,xlab='Peaks from same individual / Bin'
      ,sub=paste('Mean(homoplasy) =',round(mean(hom.vect),2),'peaks / Bin'))    
  abline(v=mean(hom.vect),col='green')
  
  hist(as.numeric(row.names(data.binary$data.binary)),freq=T,
       ,main='Bin Frequency'
       ,xlab='Bin size (bp)'
       ,ylab='Frequency (counts)'
       ,sub='In the whole dataset'
       ,col='blue')
  
  plot(density(val.height),
     col='blue',
     main='Peaks intensity',
     xlab='Peaks height)',
     xlim=c(0,2000))

  abline(v=min(val.height),col='green')
  text(x=1500,y=9*max(density(val.height)$y)/10,paste('min =',round(min(val.height2),0),'rfu'),pos=1)
         
  abline(v=mean(val.height),col='green')
  text(x=1500,y=8*max(density(val.height)$y)/10,paste('mean =',round(mean(val.height2),0),'rfu'),pos=1)
  
  abline(v=max(val.height),col='green')
  text(x=1500,y=7*max(density(val.height)$y)/10,paste('max =',round(max(val.height2),0),'rfu'),pos=1)
  }
