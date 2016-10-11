rmve <-
function (...) {
    dd = xx[, 1] <= rmx & xx[, 2] >= rmx
    if (sum(dd) == 1) {
        xx.safe <<- xx
#         meta.safe <<- meta
	numbs.safe <<- numbs
	hom.safe <<- hom
        xx <<- xx[-which(dd == T), ]
#         meta <<- meta[-which(dd == T), ]
	numbs <<- numbs[-which(dd == T)]
	hom <<- hom[-which(dd == T)]
    }
    tkrreplot(img)
}
