Spagedi.Ind <-
function(mat,path){
  n.ind=nrow(mat)
  n.pops=0
  n.spa=0
  n.loci=ncol(mat)
  n.dig=0
  n.ploid=2
  
  mat[mat==1]=2
  mat[mat==0]=1
    
  file=paste(path,.Platform$file.sep,"Spagedi.Ind-input.txt",sep='')
   
  cat(paste(n.ind,n.pops,n.spa,n.loci,n.dig,n.ploid), "\n", file=file,sep='\t')
  cat(-10, "\n", file=file,sep='\t',append=T)
  cat(c('Ind',colnames(mat)), "\n", file=file,sep='\t',append=T)
    
  data.ok=data.frame(rownames(mat),mat)
  
  BUILDit=function(vector){
    cat(unclass(vector), "\n", file=file,sep='\t',append=T)
    }
  apply(data.ok,1,BUILDit)
  cat(c('END'), "\n", file=file,sep='\t',append=T)
}
