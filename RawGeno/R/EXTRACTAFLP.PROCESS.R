EXTRACTAFLP.PROCESS <-
function(...){
## Commands associated with Buttons
    options.apply <- function(...) {
        tkconfigure(OPTIONS, cursor = "watch")
        all.dat = AFLP$all.dat
        samples.names = AFLP$samples.names
        if (tclvalue(repet.dum) == 1) {
            who.ID = as.character(tclObj(WHO.GUI))
            thresh.rep = as.numeric(tclObj(REPFR.GUI))
        }
        else {
            who.ID = "ReplicateID"
            thresh.rep = 1
        }
        if (tclvalue(keep.dum) == 1) {
            kp.val = F
        }
        else {
            kp.val = T
        }
        EXTRACTAFLP(all.dat, samples.names, TOL = .97, 
            MAXBIN = as.numeric(tclObj(MAXBIN.GUI)), MINBIN = as.numeric(tclObj(MINBIN.GUI)), 
            probs = as.numeric(tclObj(PROBS.GUI))/100, 
            freq = as.numeric(tclObj(FREQ.GUI)), who = who.ID, 
            thresh = thresh.rep, keep = kp.val, RMIN = as.numeric(tclObj(RMIN.GUI)), 
            RMAX = as.numeric(tclObj(RMAX.GUI)), cutRFU = as.numeric(tclObj(cutRFU.GUI)))
        tkdestroy(OPTIONS)
        cat("Raw Data Successfully Processed!\n")
        print(data.binary$table.stats)
        par()
        par(mfrow = c(1, 2))
        plot.gel(data.binary$data.binary, main = "Scored Gel Pseudo-Image")
        mtext("Samples", side = 1)
        mtext("Bins", side = 4)
        hist(as.numeric(row.names(data.binary$data.binary)), 
            freq = T, main = "Bin Frequency", xlab = "Bin size (bp)", 
            ylab = "Frequency (count)", sub = "In the whole dataset", 
            col = "blue")
        abline(v = as.numeric(tclObj(RMIN.GUI)), col = "green")
        abline(v = as.numeric(tclObj(RMAX.GUI)), col = "green")
    }
    options.confirm = function(...) {
        tkdestroy(OPTIONS)
    }
    
#### GUI    
    OPTIONS <- tktoplevel()
    
    tktitle(OPTIONS) <- "Aggregate AFLP Bands"
    apply.ok <- FALSE
    assign("apply.ok", apply.ok, pos = 1)
    MINBIN.GUI <- tclVar("1")
    auto.dum <- tclVar(init = "1")
    RMIN.GUI <- tclVar(paste(floor(min(AFLP$all.dat$Size))))
    RMAX.GUI <- tclVar(paste(floor(max(AFLP$all.dat$Size))))
    cutRFU.GUI <- tclVar("100")
    PROBS.GUI <- tclVar("0")
    FREQ.GUI <- tclVar("1")
    WHO.GUI <- tclVar("ReplicateID")
    REPFR.GUI <- tclVar("80")
    repet.dum <- tclVar(init = "2")
    keep.dum <- tclVar(init = "2")
    
    frame2 <- tkframe(OPTIONS, relief = "groove", borderwidth = 2)
    frame2.main <- tklabel(frame2, text = "BINNING parameters", font = "arial 11", justify = "left")
    frame2.label2 <- tklabel(frame2, text = "Maximum Bin Width (bp)", font = "arial 10")
    frame2.label3 <- tklabel(frame2, text = "Minimum Bin Width (bp)", font = "arial 10")

    frame2.entry2 <- tkentry(frame2, textvariable = MAXBIN.GUI, width = 10, justify = "left")
    frame2.entry3 <- tkentry(frame2, textvariable = MINBIN.GUI, width = 10, justify = "left")
    frame2.entry5 <- tkentry(frame2, textvariable = WHO.GUI, width = 10, justify = "left")
    tkgrid(frame2.main, columnspan = 2, sticky = "w")
    tkgrid(frame2.label2, frame2.entry2, sticky = "w")
    tkgrid(frame2.label3, frame2.entry3, sticky = "w")
    tkpack(frame2, fill = "x")
    
    frame2a <- tkframe(OPTIONS, relief = "groove", borderwidth = 2)
    frame2a.main <- tklabel(frame2a, text = "SCORING Range", font = "arial 11", justify = "left")
    frame2a.label1 <- tklabel(frame2a, text = "From (bp)", font = "arial 10")
    frame2a.label2 <- tklabel(frame2a, text = "to (bp)", font = "arial 10")
    frame2a.entry1 <- tkentry(frame2a, textvariable = RMIN.GUI, width = 10, justify = "left")
    frame2a.entry2 <- tkentry(frame2a, textvariable = RMAX.GUI, width = 10, justify = "left")
    frame2a.entry1b <- tkentry(frame2a, textvariable = tclVar(paste(min(AFLP$all.dat$Size))), width = 10, justify = "left", state = "disabled")
    frame2a.entry2b <- tkentry(frame2a, textvariable = RMAX.GUI, width = 10, justify = "left", state = "disabled")
    tkgrid(frame2a.main, columnspan = 2, sticky = "w")
    tkgrid(frame2a.label1, frame2a.entry1, frame2a.entry1b, sticky = "w")
    tkgrid(frame2a.label2, frame2a.entry2, frame2a.entry2b, sticky = "w")
    tkpack(frame2a, fill = "x")
    
    frame2b <- tkframe(OPTIONS, relief = "groove", borderwidth = 2)
    frame2b.main <- tklabel(frame2b, text = "FILTERING parameters", font = "arial 11", justify = "center")
    frame2b.label3b <- tklabel(frame2b, text = "Low Fluorescence Bins (std RFU)", font = "arial 10")
    #frame2b.label4 <- tklabel(frame2b, text = "Low Intensity Peaks (%)", font = "arial 10")
    frame2b.label5 <- tklabel(frame2b, text = "Low Frequency Bins (NbInds)", font = "arial 10")
    frame2b.label6 <- tklabel(frame2b, text = "Replicate Identifier", font = "arial 10")
    frame2b.label7 <- tklabel(frame2b, text = "Reproducibility (%)", font = "arial 10")
    frame2b.entry3b <- tkentry(frame2b, textvariable = cutRFU.GUI, width = 10, justify = "left")
    #frame2b.entry4 <- tkentry(frame2b, textvariable = PROBS.GUI, width = 10, justify = "left")
    frame2b.entry5 <- tkentry(frame2b, textvariable = FREQ.GUI, width = 10, justify = "left")
    frame2b.entry6 <- tkentry(frame2b, textvariable = WHO.GUI, width = 10, justify = "left")
    frame2b.entry7 <- tkentry(frame2b, textvariable = REPFR.GUI, width = 10, justify = "left")
    button.repet <- tkcheckbutton(frame2b, text = "Evaluate Reproducibility", width = 20, onvalue = 1, offvalue = 0, variable = repet.dum)
    button.keep <- tkcheckbutton(frame2b, text = "Eliminate untested Bins", width = 20, onvalue = 1, offvalue = 0, variable = keep.dum)
    tkgrid(frame2b.main, columnspan = 2, sticky = "w")
    tkgrid(frame2b.label3b, frame2b.entry3b, sticky = "w")
    #tkgrid(frame2b.label4, frame2b.entry4, sticky = "w")
    tkgrid(frame2b.label5, frame2b.entry5, sticky = "w")
    tkgrid(button.repet, sticky = "w")
    tkgrid(frame2b.label6, frame2b.entry6, sticky = "w")
    tkgrid(frame2b.label7, frame2b.entry7, sticky = "w")
    tkgrid(button.keep, sticky = "w")
    tkpack(frame2b, fill = "x")
    
    
    frame3 <- tkframe(OPTIONS, relief = "groove", borderwidth = 2)
    apply.but <- tkbutton(frame3, text = "START", command = options.apply, 
        padx = 40, font = "arial 10")
    confirm.but <- tkbutton(frame3, text = "Quit", command = options.confirm, 
        padx = 40, font = "arial 10")
    tkgrid(apply.but, confirm.but, sticky = "w")
    tkpack(frame3, fill = "x")
    tkbind(frame2.entry2, "<1>", tkconfigure(frame2.entry2, textvariable = MAXBIN.GUI))
}
