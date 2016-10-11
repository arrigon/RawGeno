RawGeno <-
function (...){
    if ((as.numeric(exists("AFLP")) + as.numeric(exists("mergedTable"))) == 
        0) {
        assign("dir2.GUI", 0, pos = 1)
        assign("MAXBIN2.GUI", tclVar("1.5"), pos = 1)
        assign("MAXBIN.GUI", tclVar("1.5"), pos = 1)
        assign("pop2.vect", 0, pos = 1)
        assign("matinfo", 0, pos = 1)
        assign("statexport", -9, pos = 1)
        assign("polyy", NULL, pos = 1)
    }
    else {
        usrSTART = tkmessageBox(title = "RawGeno Restarted", 
            message = "!RawGeno was restarted! \n                              \n                      \n Files from the previous session were detected, \n \n                      \n do you want to restore them?", 
            icon = "question", type = "yesno")
        if (tclvalue(usrSTART) == "no") {
            rm(AFLP, envir = .GlobalEnv)
            rm(data.binary, envir = .GlobalEnv)
            if (exists("mergedTable")) 
                rm(mergedTable, envir = .GlobalEnv)
            assign("dir2.GUI", 0, pos = 1)
            assign("MAXBIN2.GUI", tclVar("2"), pos = 1)
            assign("MAXBIN.GUI", tclVar("2"), pos = 1)
            assign("pop2.vect", 0, pos = 1)
            assign("matinfo", 0, pos = 1)
            assign("polyy", NULL, pos = 1)
            tkmessageBox(title = "RawGeno Restarted", message = "! RawGeno was reset !", 
                icon = "question", type = "ok")
        }
    }
    RG = tktoplevel()
    frame0 = tkframe(RG, borderwidth = 2)
    labelInfotitle = tklabel(frame0, text = "\n  \n  ", font = "Arial 14", 
        justify = "left")
    tkgrid(labelInfotitle, sticky = "w")
    tkpack(frame0, fill = "x")
    frame1 = tkframe(RG, borderwidth = 2)
    labelInfotitle = tklabel(frame1, text = "RawGeno Version 2.0-1", 
        font = "Arial 16", justify = "center")
    tkgrid(labelInfotitle, sticky = "w")
    tkpack(frame1, fill = "x")
    frame2 = tkframe(RG, borderwidth = 2)
    labelInfotitle = tklabel(frame2, text = "\n  RawGeno is distributed under the terms of the \n  GNU LESSER GENERAL PUBLIC LICENSE \n  Version 3, 29 June 2007.\n    \n  This software is provided without ANY WARRANTY.\n\n  Refer to documentation included within the library:\n  /R/library/RawGeno/doc\n  \n  Please check regularly \n  http://sourceforge.net/projects/rawgeno/\n  to get bug-fixes and updates.", 
        font = "Arial 9", justify = "center")
    tkgrid(labelInfotitle, sticky = "w")
    tkpack(frame2, fill = "x")
    topMenu = tkmenu(RG)
    tkconfigure(RG, menu = topMenu)
    tktitle(RG) = "RawGeno AFLP Scoring Package"
    FILES = tkmenu(topMenu, tearoff = FALSE)
    ELECTR = tkmenu(topMenu, tearoff = FALSE)
    tkadd(ELECTR, "command", label = "PeakScanner, GeneMarker or GeneScan (*.txt)", command = OPENAFLP.PROCESS)
    tkadd(FILES, "cascade", label = "Electroph.", menu = ELECTR)
    IMPRT = tkmenu(topMenu, tearoff = FALSE)
    tkadd(IMPRT, "command", label = "Single dataset (Binary table)", command = SINGLE.PROCESS)
    tkadd(IMPRT, "command", label = "Merge several datasets (Binary tables)", command = MERGING.PROCESS)
    tkadd(FILES, "cascade", label = "Import", menu = IMPRT)
    tkadd(topMenu, "cascade", label = "1. Files", menu = FILES)
    SCORING = tkmenu(topMenu, tearoff = FALSE)
    tkadd(SCORING, "command", label = "Scoring", command = EXTRACTAFLP.PROCESS)
    tkadd(topMenu, "cascade", label = "2. Scoring", menu = SCORING)
    QUAL = tkmenu(topMenu, tearoff = FALSE)
    tkadd(QUAL, "command", label = "Review Binning", command = TRACE)
    SCORQUAL = tkmenu(topMenu, tearoff = F)
    tkadd(SCORQUAL, "command", label = "Binning Diagnostics", command = QUALDIAG)
    tkadd(SCORQUAL, "command", label = "Scoring Statistics", command = SCORSTATS)
    tkadd(SCORQUAL, "command", label = "Scoring Parameters", command = SCORTECH)
    tkadd(QUAL, "cascade", label = "Scoring Quality", menu = SCORQUAL)
    SAMPLEQUAL = tkmenu(topMenu, tearoff = F)
    tkadd(SAMPLEQUAL, "command", label = "Visualize Samples", command = VISUALIZE.GUI)
    tkadd(SAMPLEQUAL, "command", label = "Export Sample Diagnostic Values", 
        command = SMPLDIAGVAL.PROCESS)
    tkadd(SAMPLEQUAL, "command", label = "PCR Plates Check", command = START.PLATES)
    tkadd(QUAL, "cascade", label = "Samples Checking", menu = SAMPLEQUAL)
    tkadd(topMenu, "cascade", label = "3. Quality Check", menu = QUAL)
    SAVE = tkmenu(topMenu, tearoff = FALSE)
    tkadd(SAVE, "command", label = "Plot PseudoGel (*.pdf) ", command = PLOT.PSEUDO.PROCESS)
    tkadd(SAVE, "command", label = "Save / Export Currently Scored Dataset", command = SAVE.GUI)
    tkadd(topMenu, "cascade", label = "4. Save", menu = SAVE)
}
