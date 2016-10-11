START.PLATES <-
function(){
  if(exists("matinfo")==F){
    err.mes()
    stop()
    } 
  if(as.numeric(is.na(match('Plate',colnames(matinfo)))==F)+
     as.numeric(is.na(match('Pos',colnames(matinfo)))==F)<2){
     err.mes()
     stop()
    }
  states=as.numeric(levels(as.factor(matinfo$Plate)))
  assign('states',states,env=.GlobalEnv)
  assign("toexport", -9, env = .GlobalEnv)
  assign("pathpdf", 0, pos = 1)
  PLATES.GUI() 
  }
