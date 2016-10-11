DELET <-
function(x){
  assign("toexport",toexport[-(x+1)],env=.GlobalEnv)
  }
