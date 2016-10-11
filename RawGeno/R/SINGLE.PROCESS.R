SINGLE.PROCESS <-
function(...){ 
  path=tclvalue(tkchooseDirectory())
  dirfiles=dir(path)
  lst=paste(path,dirfiles,sep=.Platform$file.sep)                
  assign("list.single",sort(unlist(lst)),env=.GlobalEnv)
  SINGLE()
  tkdestroy(tt)
  }
