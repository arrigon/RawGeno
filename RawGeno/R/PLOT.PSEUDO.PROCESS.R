PLOT.PSEUDO.PROCESS <-
function(...){
  all.dat=AFLP$all.dat
  peaks.info=data.binary$peaks.info

  options.apply=function(...){   
    path=tclvalue(dir.GUI)  
    plot.pseudo(all.dat,peaks.info,path,scored=T)
    cat(paste('Image Successfully Produced in ',tclvalue(dir.GUI),'\n',sep=''))
    tkdestroy(OPTIONS)
    }
    
  choose.apply=function(...){
    assign("dir2.GUI",tkchooseDirectory(),env=.GlobalEnv)
    tkdestroy(OPTIONS)
    PLOT.PSEUDO.PROCESS()
    }  
    
  quit.apply=function(...){
    tkdestroy(OPTIONS)
    }  
      
    ## GUI
  OPTIONS <- tktoplevel()
  tktitle(OPTIONS) <- "Save PseudoGel"
  if(is.numeric(dir2.GUI)==T){
    dir.GUI <- tclVar(getwd())
    } else {
    dir.GUI <- tclVar(dir2.GUI)
    }
    
  frame2 <- tkframe(OPTIONS, relief = "groove", borderwidth = 2)
  frame3 <- tkframe(OPTIONS, relief = "groove", borderwidth = 2)
    
  frame2.main <- tklabel(frame2, text = "Choose Directory",font = "arial 10", justify = "center")
  frame2.entry1 <- tkentry(frame2, textvariable = dir.GUI, width = 50, justify = "left")
    
  tkgrid(frame2.main, columnspan = 2, sticky = "w")
  tkgrid(frame2.entry1,sticky = "w")
  tkpack(frame2, fill = "x")

  ## Buttons
  save.but <- tkbutton(frame3, text = "Save", command = options.apply,padx = 40, font = "arial 10")
  browse.but <- tkbutton(frame3, text = "Browse", command = choose.apply,padx = 40, font = "arial 10")
  quit.but <- tkbutton(frame3, text = "Quit", command = quit.apply,padx = 40, font = "arial 10")

  tkgrid(save.but,browse.but,quit.but, sticky = "w")
  tkpack(frame3, fill = "x")
  }
