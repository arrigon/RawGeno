MERGING <-
function (transpose = "indRows", exclude = T, replacewith = 0){
    listfiles=list.merge
    dat=list()
    if (transpose=="indColumns") {
      for(i in 1:length(listfiles)) {
            dat[[i]]=t(read.delim(listfiles[i], row.names = 1,header = T))
        }
    } else {
        for (i in 1:length(listfiles)) {
            dat[[i]] = read.delim(listfiles[i], row.names = 1,header = T)
        }
    }
    s.names = c(0, 0)
    for(i in 1:length(listfiles)) {
        nn=data.frame(fileID = rep(i, nrow(dat[[i]])), s.names = rownames(dat[[i]]))
        s.names=rbind(s.names, nn)
    }
    s.names=s.names[-1,]
    all.samples=levels(s.names$s.names)
    matchings=c(0, 0)
    for (i in 1:length(listfiles)){
        nn=data.frame(fileID=rep(i, length(all.samples)), 
                      matchings=match(all.samples,rownames(dat[[i]])))
        matchings=rbind(matchings, nn)
    }
    matchings=matchings[-1, ]
    
    finalmat=dat[[1]][matchings[matchings$fileID == 1, 2],]
    for(i in 2:length(listfiles)){
        mat=dat[[i]][matchings[matchings$fileID == i, 2], ]
        finalmat = data.frame(finalmat, mat)
    }
    rownames(finalmat)=all.samples
    if (exclude == F){
        is.na(finalmat)=replacewith
    } else {
        test=rowSums(finalmat)
        finalmat=finalmat[is.na(test)==F,]
    }
    assign("mergedTable", finalmat, pos = 1)
    cat(paste(length(listfiles), "Files Successfully Merged! Please Use the usual saving Menu to export files!\n"))
}
