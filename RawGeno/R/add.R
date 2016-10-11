add <-
function (...) {
    xx.safe <<- xx
#     meta.safe <<- meta
    numbs.safe <<- numbs
    hom.safe <<- hom
    xx.t = rbind(xx, c(rmx - 0.5, rmx + 0.5))
#     meta.t = rbind(meta, c(0, "#009900"))
    rownames(xx.t)[nrow(xx.t)] = round(rmx, 2)
#     rownames(meta.t)[nrow(meta.t)] = round(rmx, 2)
    xx <<- xx.t
#     meta <<- meta.t
    tkrreplot(img)
}
