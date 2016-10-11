Nexus.Paup <-
function(mat,path,name='inputPaup') {
	manb <- dim(mat)[2]
 	innb <- dim(mat)[1]
	indnames <- rownames(mat)
	matm <- as.matrix(mat)

  file=paste(path,.Platform$file.sep,name,".nex",sep='')
	cat("#NEXUS", "\n", "\n", "begin taxa;", "\n", "dimensions ntax=", innb, ";", "\n", file=file, sep="")
	cat("taxlabels", "\n", file=file, sep="", append=TRUE)
	for (i in 1:innb) {
	 cat(indnames[i], "\n", file=file, sep="", append=TRUE) }
	 cat(";", "\n", "end;", "\n", "\n", file=file, sep="", append=TRUE)
	 cat("begin data;", "\n","dimensions nchar=", manb, " ntax=", innb, ";", "\n", file=file, sep="", append=TRUE)
	 cat("Format datatype=Standard symbols=", "\"", "01", "\"", ";", "\n", file=file, sep="", append=TRUE)
	 cat("matrix", "\n", file=file, sep="", append=TRUE)
   for(j in 1:innb) {
	   cat(indnames[j], "\t", matm[j , ], "\n", file=file, sep="", append=TRUE) 
     }
	 cat(";", "\n", "end;", "\n", "\n",file=file, sep="", append=TRUE)
}
