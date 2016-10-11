\name{Structure}
\alias{Structure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exporting function to Structure 2.0 }
\description{
Function which makes an input file for Structure 2.0
}
\usage{
Structure(mat, path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{ A binary matrix, individuals in rows and markers in column }
  \item{path}{ File path to produce the export }
}
\details{
According to the recommendations in the Structure manual, 
AFLPs are coded as one row with data and one row with missing
values for each individual.
}
\references{ Ehrich, D. 2006. AFLPdat: a collection of R functions for convenient handling of AFLP
data. Mol. Ecol. Notes, in press. }
\author{ Dorothée Ehrich }