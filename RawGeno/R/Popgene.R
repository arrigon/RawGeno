Popgene <-
function (mat,pops,path,filename="popgene-input.txt") {
	manb <- ncol(mat)
 	innb <- nrow(mat)
	pops <- unlist(pops)
	mat <- mat[order(pops), ]
	pops <- sort(pops)
  inds <- rownames(mat)
	
	matm <- as.matrix(mat)
	popsizes <- table(pops)
	npop <- length(popsizes)
	popnames <- vector(mode="character", npop)
	manames<-colnames(mat)
	n <- 0
	file=paste(path,.Platform$file.sep,filename,sep='')
	
	cat("/* AFLP Data Set */", "\n", "Number of populations = ", npop, "\n", file=file, sep="")
	cat("Number of loci = ", manb, "\n", "Locus name :", "\n", file=file, sep="", append=TRUE) 
  cat(manames, "\n", "\n", file=file, sep=" ", append=TRUE)   
	
	for (i in 1:npop) {
		cat("name = ", pops[n + 1], "\n", "fis = 0", "\n", file=file, sep="", append=TRUE) 	
		for (j in 1: popsizes[i]) {
			cat(matm[(n + j), ], "\n", file=file, sep="", append=TRUE)}
		cat("\n", "\n",  file=file, append=TRUE)
		popnames[i] <- pops[n+1]
		n <- n + popsizes [i]
		}
}
