SELECTFILES <-
function(){
  path=tclvalue(tkchooseDirectory())
  dirfiles=dir(path)
  lst=paste(path,dirfiles,sep=.Platform$file.sep)                
  assign("listfiles",sort(unlist(lst)),env=.GlobalEnv)
  }
