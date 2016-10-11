Treecon <-
function(mat,path){
 	manb <- dim(mat)[2]
 	innb <- dim(mat)[1]
	indnames <- rownames(mat)

 	cat(manb, "\n", file=paste(path,.Platform$file.sep,"treecon-input.txt",sep=''))
 	for (i in 1: innb) {
	cat(indnames[i], "\n", mat[i , ], "\n", file=paste(path,.Platform$file.sep,"treecon-input.txt",sep=''), append=TRUE, sep="")
		}
}
