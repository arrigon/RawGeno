Structurama <-
function(mat,path) {
	manb <- ncol(mat)
 	innb <- nrow(mat)
	indnames <- rownames(mat)
	matm <- as.matrix(mat)
	file=paste(path,.Platform$file.sep,"structurama-input.txt",sep='')
	
	cat("begin data;", "\n", "dimensions nind=", innb, " nloci=", manb, ";", "\n", "info", "\n", file=file, sep="")
	
 	for (i in 1: (innb-1)) {
 	print(indnames[i])
	   cat(indnames[i], file=file, append=TRUE, sep=" ")
        for (j in 1:manb) {
          cat("(", mat[i, j], ")", file=file, append=TRUE, sep=" ")}
     cat(",", "\n", file=file, append=TRUE, sep="")
     }
  cat(indnames[innb], file=file, append=TRUE, sep=" ")
    for (j in 1:manb) {
      cat("(", mat[i, j], ")", file=file, append=TRUE, sep=" ")}
  cat("\n", ";", "\n", "end;", file=file, append=TRUE, sep="")
 }
