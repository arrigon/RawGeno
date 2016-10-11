\name{plot.pseudo}
\alias{plot.pseudo}
\alias{PLOT.PSEUDO.PROCESS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots pseudo-gel image in a pdf file }
\description{
Internal function, produces pseudo-gel pdf.
}
\usage{
plot.pseudo(all.dat,peaks.info,path,scored=T,width=3000,height=6000,cex=.5,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{all.dat}{ all.dat object, produced by \code{\link{OPENAFLP}} }
  \item{peaks.info}{ peaks.info object, produced by \code{\link{EXTRACTAFLP}} }
  \item{path}{The file path, in order to save the pdf}
  \item{width}{ Image width (inches) }
  \item{height}{ Image heigth (inches) }
  \item{cex}{ Text size }
}
\author{ Nils Arrigo (nils.arrigo@unine.ch), University Of Neuchatel, Laboratory of Evolutive Botany, 2009 Neuchatel, Switzerland 
         http://www.unine.ch/bota/EBOLAB/}
\examples{
## Runs only if the scoring phase has been achieved.
#PLOT.PSEUDO.PROCESS()
}