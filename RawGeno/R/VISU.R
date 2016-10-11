VISU <-
function(diagno,merged,...){
  if(exists("mergedTable")){
    try(VISUALIZE(matinfo,
              column=statexport,
              columnDiagno,
              states=toexport,
              diagno,
              mergedT=T))
        } else {
    try(VISUALIZE(matinfo,
              column=statexport,
              columnDiagno,
              states=toexport,
              diagno,
              mergedT=F))        
        }
         
  }
