\name{Treecon.inv}
\alias{Treecon.inv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exporting function to Treecon, translated matrix}
\description{
Function to make an input file for Treecon (translated)
}
\usage{
Treecon(mat, path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{ A binary matrix, individuals in rows and markers in column }
  \item{path}{ File path to produce the export }
}
\details{
This function does exactly the same as the previous one (\code{\link{Treecon}}), but it creates an inverted file
with markers instead of individuals. It can be used to plot a tree where you see how
similar (and possibly linked) your markers are.
}
\references{ Ehrich, D. 2006. AFLPdat: a collection of R functions for convenient handling of AFLP
data. Mol. Ecol. Notes, in press. }
\author{ Dorothée Ehrich }