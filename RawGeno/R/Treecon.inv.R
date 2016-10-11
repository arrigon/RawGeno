Treecon.inv <-
function(mat,path) {
  mat=t(mat)
  manb <- dim(mat)[2]
 	innb <- dim(mat)[1]
	manames <- colnames(mat)

 	cat(innb, "\n", file=paste(path,.Platform$file.sep,"treecon-inv.txt",sep=''))
 	for (i in 1: manb) {
	cat(as.character(manames[i]), "\n", mat[ , i], "\n", file= paste(path,.Platform$file.sep,"treecon-inv.txt",sep=''), append=TRUE, sep="")
		}
}
