VISU.ORD <-
function(diagno,...){
if(exists("mergedTable")){
  ORDIN.INDIV(matinfo,
              column=statexport,
              columnDiagno,
              states=toexport,
              diagno=diagno,
              mergedT=T)
      } else {
  ORDIN.INDIV(matinfo,
              column=statexport,
              columnDiagno,
              states=toexport,
              diagno=diagno,
              mergedT=F)
              }
  }
