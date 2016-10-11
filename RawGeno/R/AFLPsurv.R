AFLPsurv <-
function (matm, pops, path, name="aflpdata.txt"){
    pops=unlist(pops)
    mat=matm[order(pops),]
    pops=sort(pops)
    matm=as.matrix(matm)
    npop=nlevels(as.factor(as.character(pops)))
    manames=colnames(matm)
    file = paste(path, .Platform$file.sep,name,sep = "")
    out=cbind(pops,rownames(matm),matm)
    colnames(out)=c(npop, ncol(matm), manames)
    write.table(out,file,row.names=F,col.names=T,quote=F,sep='\t')
    cat("END", file = file, sep = "", append = TRUE)
    }
