LoadBinning <-
function (){
   tkdestroy(tt)
   pop2.vect = tkgetOpenFile(title = "Select samples additionnal information file")                                                                                          
   assign("pop2.vect", pop2.vect, env = .GlobalEnv)                                                                                                                          
   xx <<- as.matrix(read.delim(tclvalue(pop2.vect), header = T))
   AllOK()
   REVIEW.GUI()
   cat("Done! Your binning was successfully loaded.","\n")
   }
