\name{vekemans.test}
\alias{vekemans.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Vekeman's Homoplasy Test }
\description{
This test is implemented in the AFLP-SURV software (Vekemans et al, 2002), it has been adapted under the R (CRAN) environment.
Internal function, used by \code{\link{QUALDIAG}}, please consult documentation.
}
\usage{
vekemans.test(data.binary = 0, bands.pos = 0, bands.freq = 0, plot = "F")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.binary}{ data.binary object, produced by \code{\link{EXTRACTAFLP}} }
  \item{bands.pos}{ bands.pos object, produced by \code{\link{QUALDIAG}} }
  \item{bands.freq}{ bands.freq object, produced by \code{\link{QUALDIAG}} }
  \item{plot}{ Provides a scatterplot of bin frequency ~ bin size}
}
\details{
Size homoplasy is expected to occur when a large number of fragments is amplified for a given primer combination, and will be higher for small
fragments because of the non-uniform distribution of fragment sizes under AFLP (Vekemans et al. 2002). As a
consequence, a negative correlation between fragment size and frequency will be observed if size homoplasy
occurs (Vekemans et al. 2002).(from the AFLP-SURV documentation)
The calculations are done by the \code{\link{cor.test}} function.
}
\value{
a dataframe displaying:
  The pearson's correlation cor.test(bin frequency, bin size)
  And its associated P.value (t distribution)
}
\references{ Vekemans, Baauwens et al, Molecular Ecology (2002) 11,139-151
             Vekemans, X. 2002. AFLP-SURV version 1.0. Distributed by the author. Laboratoire de Génétique et Ecologie
Végétale, Université Libre de Bruxelles, Belgium. }
\author{ Nils Arrigo (nils.arrigo@unine.ch), University Of Neuchatel, Laboratory of Evolutive Botany, 2009 Neuchatel, Switzerland 
         http://www.unine.ch/bota/EBOLAB/}