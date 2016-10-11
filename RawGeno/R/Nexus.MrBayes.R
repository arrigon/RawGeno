Nexus.MrBayes <-
function(mat,path,name='inputMrBayes'){
	manb=dim(mat)[2]
 	innb=dim(mat)[1]
	indnames=rownames(mat)
	matm=as.matrix(mat)

  file=paste(path,.Platform$file.sep,name,'.nex',sep='')
	cat("#NEXUS", "\n", file=file, sep="")
  cat("begin data;", "\n","dimensions nchar=", manb, " ntax=", innb, ";", "\n", file=file, sep="", append=TRUE)
  cat("Format datatype=restriction missing=N;", "\n", file=file, sep="", append=TRUE)
  cat("matrix", "\n", file=file, sep="", append=TRUE)
  for(j in 1:innb) {
    cat(indnames[j], "\t", matm[j , ], "\n", file=file, sep="", append=TRUE) 
    }
  cat(";", "\n", "end;", "\n", "\n",file=file, sep="", append=TRUE)
  cat("begin mrbayes;", "\n","outgroup OUTGROUP_TO_BE_ADDED", ";", "\n", "Lset coding=noabsencesites;", "\n","mcmc ngen=20000000 printfreq=1000 samplefreq=1000 savebrlens=yes nruns=2 nchains=2;", "\n","end;",file=file,append=T) 
  }
