Mltr <-
function(mat,pops,path){
  pops <- unlist(pops)
  mat <- mat[order(pops),] 
  pops <- sort(pops)
  
  mat[mat==0]=2
  data.ok=cbind(pops,pops,mat)
  
  file=paste(path,.Platform$file.sep,"mltr-input.txt",sep='')
   
  cat(paste(ncol(mat),1,0,sep=','), "\n", file=file,sep='')
  cat(colnames(mat), "\n", file=file,sep=',',append=T)
  
  BUILDit=function(vector){
    cat(unclass(vector), "\n", file=file,sep=',',append=T)
    }
  apply(data.ok,1,BUILDit)
}
