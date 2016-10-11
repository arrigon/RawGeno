\name{Structure.D}
\alias{Structure.D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exporting function to Structure 2.2 }
\description{
Function which makes an input file for Structure version 2.2
}
\usage{
Structure.D(mat, path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{ A binary matrix, individuals in rows and markers in column }
  \item{pops}{ A vector, containing population belonging information. length(pops)=nrow(mat) }
  \item{path}{ File path to produce the export }
}
\details{
This function treats dominant data explicitly. All loci are coded as dominant in the input.
}
\references{ Ehrich, D. 2006. AFLPdat: a collection of R functions for convenient handling of AFLP
data. Mol. Ecol. Notes, in press. }
\author{ Nils Arrigo (nils.arrigo@unine.ch), University Of Neuchatel, Laboratory of Evolutive Botany, 2009 Neuchatel, Switzerland 
         http://www.unine.ch/bota/EBOLAB/}