\name{AFLPsurv}
\alias{AFLPsurv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exporting function to AFLP-SURV }
\description{
Function to make an input file for AFLPsurv
}
\usage{
AFLPsurv(mat, pops, path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{ A binary matrix, individuals in rows and markers in column }
  \item{pops}{ A vector, containing population belonging information. length(pops)=nrow(mat) }
  \item{path}{ File path to produce the export }
}
\references{ Ehrich, D. 2006. AFLPdat: a collection of R functions for convenient handling of AFLP
data. Mol. Ecol. Notes, in press. }
\author{ Dorothée Ehrich }