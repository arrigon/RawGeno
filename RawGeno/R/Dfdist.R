Dfdist <-
function(mat,pops,path=getwd()) {
	pops=unlist(pops)
	mat=mat[order(pops),]
	pops=as.factor(sort(pops))
	matm=as.matrix(mat)

  tot=aggregate(matm,list(pops),length)[,-1]
  all.1=aggregate(matm,list(pops),sum)[,-1]
  all.0=tot-all.1

  file=paste(path,.Platform$file.sep, "DfdistInfile.txt",sep='')
  cat(0,"\n", file=file,sep="\t")
  cat(nlevels(pops),"\n", file=file,append=TRUE,sep="\t")
  cat(ncol(matm),"\n","\n",file=file,append=TRUE,sep="\t")
  for (i in 1:ncol(all.1)) {
  	cat(2,"\n", file=file,append=TRUE,sep="\t")
  	write.table(cbind(all.1[,i],all.0[,i]),file=file,append=TRUE,sep="\t",row.names=F,col.names=F)
  	cat("\n", file=file, append=TRUE)
    }	
  }
