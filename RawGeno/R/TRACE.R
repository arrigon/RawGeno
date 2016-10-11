TRACE <-
function (...){      
    all.dat<<-AFLP$all.dat
    all.dat$Peak_area[all.dat$Peak_area==0]=1
    all.dat$Peak_width[all.dat$Peak_width==0]=0.05

    if (is.null(polyy)) {                                                                                                                          
        quit.fun = function(...) {                                                                                                                 
            stop("Binning review stopped\n")                                                                                                       
            tkdestroy(tt)                                                                                                                          
        }     
                                                                                                                   
        start.fun = function(...) {                                                                                                                
            cat("Start computing electropherograms (please wait)...\n")      
	    scal = seq(min(all.dat$Size), max(all.dat$Size), by = 0.05)                                                                    
            polyy = scal
	    samps=levels(as.factor(all.dat$sample.id))

	    ## Loop for drawing the electrophoretic curves
	    nsamps=round(length(samps)*as.numeric(tclObj(PROP.GUI))/100,0)
	    for(i in sample(samps,nsamps)){
	      all.dat.p=all.dat[all.dat$sample.id==i,]
	      all.dat.p=all.dat.p[is.na(all.dat.p$Peak_height)==F,]
	      mus=all.dat.p$Size
	      amps=all.dat.p$Peak_height
	      surfs=all.dat.p$Peak_area
	      sigms=all.dat.p$Peak_width/2.3548
	      norm=amps/(sigms*sqrt(2*pi))
	      div=(2*sigms^2)
	      draw=function(x,...){
		exx=-((x-mus)^2)/div
		sum(norm*exp(exx))
		}
	      polyy=cbind(polyy,sapply(scal,draw))
	      }
            assign("polyy", polyy, env = .GlobalEnv)                                                                                               
            cat("Start computing summary electropherogram...done\n")                                                                               
            tkdestroy(tt)                                                                                                                          
            REVIEW.GUI()                                                                                                                           
        }                
	PROP.GUI <- tclVar("50")
        tt = tktoplevel()                                                                                                                          
        tktitle(tt) = "Review binning"                                                                                                             
        frame1 = tkframe(tt, relief = "groove", borderwidth = 2)                                                                                   
        labelInfotitle = tklabel(frame1, text = "      \n    \n    RawGeno has first to compute the electropherograms.\n    This step might require several minutes.\n",                       
            font = "arial 10", justify = "left")   
        labelEntry = tklabel(frame1, text = "      \n    \n    Proportion of sampling being displayed (%) ?\n",                       
            font = "arial 10", justify = "left") 
	entry <- tkentry(frame1, textvariable = PROP.GUI, width = 10, justify = "left")                                                                                                 
        tkgrid(labelInfotitle, sticky = "w")  
	tkgrid(labelEntry,entry, sticky = "w")                                                                              
        tkpack(frame1, fill = "x", side = "top")
        frame2 = tkframe(tt, relief = "groove", borderwidth = 2)
        browse.pop = tkbutton(frame2, text = "START", command = start.fun,
            padx = 20, font = "arial 10")
        quit.but = tkbutton(frame2, text = "QUIT", command = quit.fun,
            padx = 20, font = "arial 10")
        tkgrid(browse.pop, quit.but, sticky = "w")
        tkpack(frame2, fill = "x", side = "bottom")
    }
    else {
        REVIEW.GUI()
    }
}
