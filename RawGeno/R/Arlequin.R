Arlequin <-
function (mat,pops,path) {
  pops=unlist(pops)
  manb <- dim(mat)[2]
 	innb <- dim(mat)[1]

	mat <- mat[order(pops), ]
	pops <- sort(pops)
  inds <- rownames(mat)
	matm <- as.matrix(mat)
	popsizes <- table (pops)
	npop <- length(popsizes)
	popnames <- vector(mode="character", npop)
	n <- 0

  file=paste(path,.Platform$file.sep,"arlequin-input.arp",sep='')
	cat("[Profile]", "\n", "Title=", "\"", "AFLP data", "\"", "\n", "NbSamples=", file=file, sep="")
	cat(npop, "\n", "DataType=RFLP", "\n", "GenotypicData=0", "\n", "LocusSeparator=NONE", "\n", file=file, sep="", append=TRUE)
	cat("CompDistMatrix=0", "\n", "\n", file=file, append=TRUE)
	cat("[Data]", "\n", "[[Samples]]", "\n", "\n", file=file, append=TRUE)

	for (i in 1:npop) {
		cat("SampleName=", "\"", pops[n + 1], "\"", "\n", file=file, sep="", append=TRUE)
		cat("SampleSize=", popsizes[i], "\n", file=file, append=TRUE)
		cat("SampleData= {", "\n", file=file, append=TRUE)
		for (j in 1: popsizes[i]) {
			cat(inds[n + j], "\t", "1", "\t", matm[(n + j), ], "\n", file=file, append=TRUE)}
		  cat("}", "\n", "\n",  file=file, append=TRUE)
		  popnames[i] <- pops[n+1]
		  n <- n + popsizes [i]
		  }

	cat("[[Structure]]", "\n", "\n", "StructureName = ", "\"", "one group", "\"", "\n", "NbGroups = 1", "\n", file=file, sep="", append=TRUE)
	cat("\n", "Group ={", "\n", file=file, sep="", append=TRUE)
	for (i in 1:npop) {
		cat("\"", popnames[i], "\"", "\n", file=file, append=TRUE, sep="")
    }
    cat("}", "\n", file=file, append=TRUE)
}
