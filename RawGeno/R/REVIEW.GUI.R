REVIEW.GUI <-
function (...) {
    peaks.info <<- data.binary$peaks.info
    modified <<- 0
    if (!require(tkrplot)) {
        quit.apply = function(...) {
            tkdestroy(tt)           
        }                           
        install.apply = function(...) {
            install.packages("tkrplot")
            tkdestroy(tt)              
        }                              
        tt = tktoplevel()              
        tktitle(tt) = "Review Binning" 
        frame1 = tkframe(tt, relief = "groove", borderwidth = 2)
        labelInfotitle = tklabel(frame1, text = "\n    RawGeno now attempts to load\n    a binning editing device.\n\n    This latter depends on the package tkrplot,\n    which is currently missing from your computer.\n                 \n    This package can be installed now with your permission,\n    shall we proceed ?")                                   
        tkgrid(labelInfotitle, sticky = "w")                                                                                                                                       
        tkpack(frame1, fill = "x", side = "top")                                                                                                                                   
        frame2 = tkframe(tt, relief = "groove", borderwidth = 2)                                                                                                                   
        browse.pop = tkbutton(frame2, text = "Install tkrplot",                                                                                                                    
            command = install.apply, padx = 20, font = "arial 10")                                                                                                                 
        quit.but = tkbutton(frame2, text = "Skip samples filtering",                                                                                                               
            command = quit.apply, padx = 20, font = "arial 10")                                                                                                                    
        tkgrid(browse.pop, quit.but, sticky = "w")                                                                                                                                 
        tkpack(frame2, fill = "x", side = "bottom")                                                                                                                                
        stop()                                                                                                                                                                     
        tkdestroy(tt)                                                                                                                                                              
    }                                                                                                                                                                              
    y = polyy                                                                                                                                                                      
    x = polyy[, 1]                                                                                                                                                                 
    hscale = 2.3                                                                                                                                                                   
    vscale = 1.2                                                                                                                                                                   
    wait = F                                                                                                                                                                       
    yy <<- c(0, max(y) - 20)                                                                                                                                                       
    first <<- TRUE                                                                                                                                                                 
    ul <<- 0                                                                                                                                                                       
    ur <<- 0                                                                                                                                                                                                                                                                                                             
    data.size = data.binary$data.size                                                                                                                                              
    data.size[data.size == 0] = NA                                                                                                                                                 
    data.size = data.size                                                                                                                                                        
    hom = data.binary$data.hom                                                                                                                                                     
    hom[hom == 0] = NA                                                                                                                                                             
    numbs <<- rowSums(data.binary$data.binary, na.rm = T)                                                                                                                          
    hom <<- rowMeans(hom, na.rm = T)
    
    if(!exists("xx") | autoscoring == 1){
      mins = aggregate(peaks.info[,1],by=list(cumsum(peaks.info[,3])),min)[,2]
      maxs = aggregate(peaks.info[,1],by=list(cumsum(peaks.info[,3])),max)[,2]
      status = aggregate(peaks.info[,6],by=list(cumsum(peaks.info[,3])),max)[,2]
#       cols = status
#       cols[cols == 0] = "#009900"
#       cols[cols != "#009900"] = grey(0.9)
#       meta <<- data.frame(status, cols)
      xx = cbind(mins, maxs)
      xx <<- xx[status == 0, ]
      }

    initf <<- 100                                                                                                                                                                  
    initz <<- 5                                                                                                                                                                    
    initspan <<- 2 * initz                                                                                                                                                         
    yzm <<- tclVar()                                                                                                                                                               
    tclvalue(yzm) = max(y)                                                                                                                                                         
    zoom <<- tclVar()                                                                                                                                                              
    tclvalue(zoom) = initz                                                                                                                                                         
    focus <<- tclVar()                                                                                                                                                             
    tclvalue(focus) = initf                                                                                                                                                        
    rbValue <<- tclVar("View")                                                                                                                                                     
    tt <<- tktoplevel()                                                                                                                                                            
    tkwm.title(tt, "Review Binning")                                                                                                                                               
    img <<- tkrplot(tt, replot, vscale = vscale, hscale = hscale)                                                                                                                  
    ver = tkscale(tt, variable = yzm, orient = "vertical", command = function(...) tkrreplot(img),                                                                                 
        from = 100, to = max(y) + 500, resolution = 0.5)                                                                                                                           
    tkconfigure(ver, showvalue = 0)                                                                                                                                                
    hor = tkscale(tt, variable = focus, orient = "horizontal",                                                                                                                     
        command = function(...) tkrreplot(img), from = 50, to = max(x),                                                                                                            
        resolution = 0.5)                                                                                                                                                          
    zm = tkscale(tt, variable = zoom, orient = "horizontal",                                                                                                                       
        command = function(...) tkrreplot(img), from = 5, to = 15,                                                                                                                 
        resolution = 0.5)                                                                                                                                                          
    tkpack(ver, side = "left")                                                                                                                                                     
    tkpack(img, hor, side = "top", fill = "x")                                                                                                                                     
    tkpack(zm, side = "top")                                                                                                                                                       
    rb1 = tkradiobutton(tt)                                                                                                                                                        
    rb2 = tkradiobutton(tt)                                                                                                                                                        
    tkconfigure(rb1, variable = rbValue, value = "View")                                                                                                                           
    tkconfigure(rb2, variable = rbValue, value = "Edit")
    tkpack(tklabel(tt, text = "Display mode: ", font = "Arial 10 bold"), side = "left")
    tkpack(tklabel(tt, text = "View", font = "Arial 10"), rb1, side = "left")                                                                                                                                                        
    tkpack(tklabel(tt, text = "Edit", font = "Arial 12"), rb2, side = "left") 
    tkpack(tklabel(tt, text = "       Binning file:", font = "Arial 10 bold"), side = "left")
    tkpack(tkbutton(tt, text = "Import former binning", command = LoadBinning, font = "Arial 10"), side = "left")  
    tkpack(tkbutton(tt, text = "Save current binning", command = SaveBinning, font = "Arial 10"), side = "left")                                                                                                                                                        
    tkpack(tkbutton(tt, text = "Finished!", command = AllOK, font = "Arial 12"), side = "right")                                                                                                                                        
    tkpack(tkbutton(tt, text = "Cancel", command = function() tkdestroy(tt), font = "Arial 12"), side = "right")                                                                                                                                        
    md <<- FALSE                                                                                                                                                                   
    iw <<- as.numeric(tcl("image", "width", tkcget(img, "-image")))                                                                                                                
    ih <<- as.numeric(tcl("image", "height", tkcget(img, "-image")))                                                                                                               
    ccx <<- ccy <<- 0                                                                                                                                                              
    ci <<- 0                                                                                                                                                                       
    tkbind(img, "<Motion>", mouse.move)
    tkbind(img, "<ButtonPress-1>", mouse.down)
    tkbind(img, "<ButtonRelease-1>", mouse.up)
    txt <<- tktext(img)
    editPopupMenu <<- tkmenu(txt, tearoff = FALSE)
    tkadd(editPopupMenu, "command", label = "Add Bin", command = add)
    tkadd(editPopupMenu, "command", label = "Remove Bin", command = rmve)
    tkadd(editPopupMenu, "command", label = "Cancel last action",
        command = cancel)
    tkbind(img, "<Button-3>", RightClick)
}
