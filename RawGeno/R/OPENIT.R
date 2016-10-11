OPENIT <-
function(dyecol='B'){  
    ### Create Main Index    
    if(length(listfiles)==1){
      test = scan(listfiles, what = 'complex')
      if(any(grep('GeneMarker', test))){
	AFLP = OPENAFLP(listfiles, dyecol, format = 'GeneMarker')
	} else {
	AFLP = OPENAFLP(listfiles, dyecol, format = 'pksc')
	}
      cat(paste(length(AFLP$samples.names), 'Samples Successfully Imported!'))

      } else {    
      AFLP = try(OPENAFLP(listfiles, dyecol, format = 'GS'), silent=T)
      chck = row.names(as.matrix(table(AFLP$all.dat[,1])))
    
    if(length(chck)<length(listfiles)){
      tocheck=c(1:length(listfiles))[is.na(match(1:length(listfiles),as.numeric(chck)))]
      filetocheck=as.vector(basename(listfiles[tocheck]))
      write.table(filetocheck,paste(dirname(listfiles[1]),.Platform$file.sep,'ErrorReport.csv',sep=''),sep=';')
      cat(paste('Errors during importation, please consult "ErrorReport.csv" saved in GeneScan files directory'))                                                                                            
      AFLP$pops=AFLP$pops[-tocheck]
      AFLP$samples.names=AFLP$samples.names[-tocheck]
      assign("AFLP", AFLP, env = .GlobalEnv)
      } else {
      cat(paste(length(AFLP$samples.names),'Samples Successfully Imported! \n'))
      }
    }
  }
