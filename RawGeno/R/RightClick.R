RightClick <-
function (x, y) {               
    if (tclvalue(rbValue) == "Edit") {
        modified <<- 1         
        autoscoring <<- 0   

        rootx <- as.integer(tkwinfo("rootx", img))
        rooty <- as.integer(tkwinfo("rooty", img))
        xTxt <<- as.integer(x) + rootx            
        yTxt <<- as.integer(y) + rooty            
        tcl("tk_popup", editPopupMenu, xTxt, yTxt)
        shift <<- (mean(c(ur, ul)) - initf)       
        deltaU <<- abs(ur - ul)                   
        PltoU <<- deltaU/initspan
        zmu <<- zms * PltoU
        centerscreen <<- mean(c(ur, ul)) + (focs - initf)
        urx <<- centerscreen + zmu
        ulx <<- centerscreen - zmu
        tx <<- (as.numeric(x) - 1)/iw
        rmx <<- tx * urx + (1 - tx) * ulx
    }
}
