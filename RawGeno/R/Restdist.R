Restdist <-
function(mat,path,labels,n.enzymes=2,trim=10,filename='Restdist.phy'){
  n.ind=nrow(mat)
  n.loci=ncol(mat)
  
  file=paste(path,.Platform$file.sep,filename,sep='')
  cat(paste(n.ind,n.loci,n.enzymes), "\n", file=file,sep='\t')
  for(i in 1:nrow(matm)){
    lineprint=paste(c(strtrim(paste(i,labels[i],"___________"),trim),mat[i,]),collapse='')
    cat(lineprint, "\n", file=file,append=T)
    }  
  }
