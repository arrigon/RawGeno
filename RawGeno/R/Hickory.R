Hickory <-
function (mat,pops,path) {
	pops=unlist(pops)
  manb <- dim(mat)[2]
 	innb <- dim(mat)[1]
	mat <- mat[order(pops), ]
	pops <- sort(pops)
  inds <- rownames(mat)
	matm <- as.matrix(mat)
	popsizes <- table(pops)
	npop <- length(popsizes)
	popnames <- vector(mode="character", npop)
	n <- 0
  
  file=paste(path,.Platform$file.sep,"Hickory-input.nex",sep='')
	cat("#NEXUS", "\n", "begin alleles;", "\n", "dimensions newpops nloci=", manb, " npops=", npop, ";", "\n", file=file, sep="")
	cat("dominant;", "\n", "format labels missing=? ;", "\n", "matrix", "\n", file=file, sep="", append=TRUE)

	for (i in 1:npop) {
		if (n != 0) cat(",", "\n", file=file, sep="", append=TRUE)
		cat(pops[n + 1], ":", "\n", file=file, sep="", append=TRUE)
		for (j in 1: popsizes[i]) {
			cat(inds[n + j], "\t", matm[(n + j), ], "\n", file=file, append=TRUE)}
		popnames[i] <- pops[n+1]
		n <- n + popsizes [i]
	}

	cat(";", "\n", "end;", "\n", "\n", file=file, append=TRUE)
}
