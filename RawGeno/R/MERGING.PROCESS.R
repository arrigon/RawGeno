MERGING.PROCESS <-
function(...){ 
  path=tclvalue(tkchooseDirectory())
  dirfiles=dir(path)
  lst=paste(path,dirfiles,sep=.Platform$file.sep)                
  assign("list.merge",sort(unlist(lst)),env=.GlobalEnv)
  SEVERALMERGE()
  tkdestroy(tt)
  }
