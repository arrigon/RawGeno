Structure.popsD <-
function (mat, pops, path, flag=0, name = "STR2.2.P"){
    pops = as.numeric(as.factor(as.character(pops)))
    manb <- ncol(mat)
    innb <- nrow(mat)
    mat <- mat[order(pops),]
    if (flag!=0) flag <- flag[order(pops)]
    pops <- sort(pops)
    inds <- rownames(mat)
    matm <- as.matrix(mat)
    indnames <- rownames(mat)
    matm <- as.matrix(mat)
    file.name = paste(path,.Platform$file.sep, name, nrow(matm), 'ind', ncol(matm), "loc.txt", sep = "")
    cat(colnames(mat), file = file.name, sep = "\t")
    cat(c(rep(0, manb)), file = file.name, sep = "\t")
    cat("\n", file = file.name, append = TRUE)
    if(length(flag)==1){
      for (i in 1:innb) {
          cat(indnames[i], pops[i], matm[i, ], "\n", file = file.name,
              append = TRUE, sep = "\t")
      }
    } else {
      for (i in 1:innb) {
          cat(indnames[i], pops[i], flag[i], matm[i, ], "\n", file = file.name,
              append = TRUE, sep = "\t")
      }    
    } 
}
