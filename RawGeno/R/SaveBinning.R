SaveBinning <-
function (){                                                                                                                                                                                  
    path = tclvalue(tkchooseDirectory(title = "Select directory for saving Binning files"))   
    write.table(xx, paste(path, .Platform$file.sep, "BinningFile_",Sys.Date(),".txt", sep = ""), sep = "\t", row.names = F)
    cat("Done! The current binning was saved in", paste(path, .Platform$file.sep, "BinningFile_",Sys.Date(),".txt", sep = ""),"\n")
    }
