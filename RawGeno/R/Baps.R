Baps <-
function(mat,path) {
	manb <- dim(mat)[2]
 	innb <- dim(mat)[1]
	indnames <- rownames(mat)
	matm <- as.matrix(mat)

 	for (i in 1: innb) {
	cat(matm[i , ], i, "\n", file= paste(path,.Platform$file.sep,"baps-input.txt",sep=''), append=TRUE, sep="\t")
	}
}
