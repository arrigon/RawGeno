\name{CLEAN}
\alias{CLEAN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal: cleaning AFLP profiles just after import.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CLEAN(AFLP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AFLP}{
%%     ~~Describe \code{AFLP} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (AFLP) 
{
    if (!require(tkrplot)) {
        quit.apply = function(...) {
            tkdestroy(tt)
        }
        install.apply = function(...) {
            install.packages("tkrplot")
            tkdestroy(tt)
        }
        tt = tktoplevel()
        tktitle(tt) = "Filter low quality samples"
        frame1 = tkframe(tt, relief = "groove", borderwidth = 2)
        labelInfotitle = tklabel(frame1, text = "\n    RawGeno now attempts to load\n    a sample filtering device.\n\n    This latter depends on the package tkrplot,\n    which is currently missing from your computer.\n                 \n    This package can be installed now with your permission,\n    shall we proceed ?")
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
    all.dat = AFLP$all.dat
    focus = all.dat[all.dat$Size > 100 & all.dat$Size < 500, 
        ]
    assign("npks", aggregate(all.dat$Size, by = list(all.dat$sample.id), 
        length), env = .GlobalEnv)
    x = 1:length(npks$x)
    y = sort(npks$x)
    snap.to.x = T
    snap.x <- tclVar()
    tclvalue(snap.x) <- ifelse(snap.to.x, "T", "F")
    hscale = 2
    vscale = 1.3
    wait = F
    xxx = as.numeric(x)
    ax = min(x)
    cx = max(x)
    xx = c(ax, cx)
    af = approxfun(x, y)
    ay = af(ax)
    cy = af(cx)
    yy = c(ay, cy)
    first = TRUE
    ul = ur = 0
    stats = quantile(y, c(0.05, 0.5, 0.95))
    replot = function(...) {
        par(mar = c(5, 4, 4, 4) + 0.1)
        u = par("usr")
        plot(x, y, type = "b", ylab = "AFLP peaks per Individual", 
            xlab = "Individuals (sorted according to their number of AFLP peaks)")
        polygon(c(xx[1], xx[2], xx[2], xx[1]), c(yy[2], yy[2], 
            yy[1], yy[1]), col = "#009900")
        text(xx[2], yy[1] + 2, "Individuals conserved for further analysis", 
            pos = 2, col = "light green")
        abline(h = stats[1], lty = 2)
        abline(h = stats[2], lty = 2)
        abline(h = stats[3], lty = 2)
        mtext(paste(c(5, 50, 95), "\%"), side = 4, at = stats, 
            cex = 0.8)
        mtext(paste(abs(diff(xx) + 1), "Individuals kept"), side = 1, 
            at = length(x), line = 2, cex = 0.8, col = "red")
        mtext(paste(round(100 * abs(diff(xx) + 1)/length(xxx), 
            0), "\%", "Sampling kept"), side = 1, at = length(x), 
            line = 3, cex = 0.8, col = "red")
        points(x, y, pch = 16)
        mtext(yy, side = 4, at = yy, line = 2, col = "#009900")
        if (first) {
            first <<- FALSE
            tmp = cnvrt.coords(c(0, 1), c(0, 1), input = "dev")$usr
            ul <<- tmp$x[1]
            ur <<- tmp$x[2]
        }
        assign("out", yy, env = .GlobalEnv)
    }
    MODIF = function() {
        out = sort(out)
        tokeep = npks[which(npks$x >= out[1] & npks$x <= out[2]), 
            ]
        AFLP$all.dat = AFLP$all.dat[is.na(match(AFLP$all.dat$sample.id, 
            tokeep[, 1])) == F, ]
        AFLP$all.dat$sample.id = as.factor(AFLP$all.dat$sample.id)
        levels(AFLP$all.dat$sample.id) = 1:nrow(tokeep)
        AFLP$all.dat$sample.id = as.numeric(AFLP$all.dat$sample.id)
        lostsamples = AFLP$samples.names[-tokeep[, 1]]
        AFLP$samples.names = AFLP$samples.names[tokeep[, 1]]
        assign("AFLP", AFLP, env = .GlobalEnv)
        cat("\nDone, low quality samples were removed accordingly.\n")
        cat("See in working directory for a list of samples that were removed\n")
        cat("Samples removed during importation step:\n", file = "LostSamplesScoring.txt")
        write.table(lostsamples, "LostSamplesScoring.txt", sep = "\t", 
            col.names = F, append = T)
        tkdestroy(tt)
    }
    tt <- tktoplevel()
    tkwm.title(tt, "Filter low quality samples")
    labelInfotitle = tklabel(tt, text = "\n    This device helps removing low quality samples from your analysis, individuals are displayed according to their number of successfully amplified AFLP bands.     \n    Low quality reactions generally produce less AFLP bands than successfull reactions: this provides a reliable way to\n    filter individuals according to their quality before performing scoring.\n                \n    Click-and-drag the green area over individuals you want to include into your dataset.", 
        font = "Arial 9", justify = "left")
    tkpack(labelInfotitle)
    img <- tkrplot(tt, replot, vscale = vscale, hscale = hscale)
    tkpack(img, side = "top")
    tkpack(tkbutton(tt, text = "Quit and skip filtering", command = function() tkdestroy(tt), 
        font = "Arial 9"), side = "right")
    tkpack(tkbutton(tt, text = "START", command = MODIF, font = "Arial 9"), 
        side = "right")
    md <- FALSE
    iw <- as.numeric(tcl("image", "width", tkcget(img, "-image")))
    ih <- as.numeric(tcl("image", "height", tkcget(img, "-image")))
    ccx <- ccy <- 0
    ci <- 0
    mouse.move <- function(x, y) {
        if (md) {
            tx <- (as.numeric(x) - 1)/iw
            ccx <<- tx * ur + (1 - tx) * ul
            if (as.logical(tclvalue(snap.x))) {
                ccx <<- xxx[which.min(abs(ccx - xxx))]
            }
            xx[ci] <<- ccx
            ccy <<- af(ccx)
            yy[ci] <<- ccy
            tkrreplot(img)
        }
    }
    mouse.down = function(x, y) {
        tx = (as.numeric(x) - 1)/iw
        txx = tx * ur + (1 - tx) * ul
        ci <<- which.min(abs(txx - xx))
        md <<- TRUE
        mouse.move(x, y)
    }
    mouse.up <- function(x, y) {
        md <<- FALSE
    }
    tkbind(img, "<Motion>", mouse.move)
    tkbind(img, "<ButtonPress-1>", mouse.down)
    tkbind(img, "<ButtonRelease-1>", mouse.up)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
