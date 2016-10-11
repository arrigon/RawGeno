Network <-
function(mat,labs=rownames(mat),path=getwd()){ 
  mat=matm
  labs=rownames(mat)
  mat=mat[,colSums(mat)>1|colSums(mat)<nrow(mat)]
  nmarks=ncol(mat)
  marks=1:nmarks
  NUMLINES=nchar(as.character(nmarks))
  
  maxname=max(nchar(labs))
  
  file=paste(path,.Platform$file.sep,"network-input.rdf",sep='')
  
  cat("  ;1.0","\n",sep='',file=file)
  cat(paste(paste(marks,";",sep=''),sep='',collapse=''),"\n",sep='',file=file,append=T)
  cat(paste(rep("10",nmarks),";",sep='',collapse=''),"\n",sep='',file=file,append=T)
 
  for(i in 1:nrow(mat)){
    nams=paste(">",labs[i],";1;",sep='')
    liner=paste(mat[i,],collapse='')
    cat(nams,"\n",sep='',file=file,append=T)
    cat(liner,"\n",sep='',file=file,append=T)
    }
}
