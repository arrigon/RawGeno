\name{Hickory}
\alias{Hickory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exporting function to Hickory }
\description{
Function to make an input file for Hickory
}
\usage{
Hickory(mat, pops, path)
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