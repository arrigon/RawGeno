SMPLDIAGVAL.PROCESS <-
function(){
  path=tclvalue(tkchooseDirectory())
  assign('pathVal',path,env=.GlobalEnv)
  SMPLDIAGVAL(pathVal)
  }
