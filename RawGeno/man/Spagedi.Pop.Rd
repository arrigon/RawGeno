\name{Spagedi.Pop}
\alias{Spagedi.Pop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exporting function to Spagedi - Populations level }
\description{
Function to make an input file for Spagedi, at the population level
}
\usage{
Spagedi.Pop(mat, pops, path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{ A binary matrix, individuals in rows and markers in column }
  \item{pops}{ A vector, containing population belonging information. length(pops)=nrow(mat) }
  \item{path}{ File path to produce the export }
}
\references{ Ehrich, D. 2006. AFLPdat: a collection of R functions for convenient handling of AFLP
data. Mol. Ecol. Notes, in press. }
\author{ Doroth�e Ehrich }