Structure.Dhapl <-
function(mat,path,name='STR_hapl') {
	manb <- ncol(mat)
 	innb <- nrow(mat)
	indnames <- rownames(mat)
	matm <- as.matrix(mat)
	
  file.name = paste(path, .Platform$file.sep, name,nrow(matm),'ind',ncol(matm),"loc.txt", sep = "")
	cat(colnames(mat), file=file.name,  sep="\t")
  cat(c(rep(0, manb)), file=file.name,  sep="\t")
	cat("\n", file=file.name,append=TRUE)
 	for (i in 1:innb) {
	cat(indnames[i], matm[i , ], "\n", file= file.name, append=TRUE, sep="\t")	
	}
}
