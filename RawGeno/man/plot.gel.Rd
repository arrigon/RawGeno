\name{plot.gel}
\alias{plot.gel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot a matrix }
\description{
Internal function, plots pseudo-gels on the screen.
}
\usage{
plot.gel(x, nrgcols = 2, rlabels = FALSE, clabels = FALSE, rcols = 1, ccols = 1, title = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Matrix }
  \item{nrgcols}{ the number of colors (>= 1) to be used in the red and black palette. }
  \item{rlabels}{ vector of character strings to be placed at the row tickpoints, labels for the rows of X. }
  \item{clabels}{ vector of character strings to be placed at the column tickpoints, labels for the columns of X. }
  \item{rcols}{ colors to be used for the labels of the rows of X. rcols can have either length 1, in which case all the labels are displayed using the same color, or the same length as rlabels, in which case a color is specified for the label of each row of X. }
  \item{ccols}{ 	colors to be used for the labels of the columns of X. ccols can have either length 1, in which case all the labels are displayed using the same color, or the same length as clabels, in which case a color is specified for the label of each column of X. }
  \item{title}{ character string, overall title for the plot. }
  \item{\dots}{ graphical parameters may also be supplied as arguments to the function (see par). E.g. zlim=c(-3,3) }
}
\details{
Adapted from the function plot.mat (sma). 
}
\author{ Sandrine Dudoit, sandrine@stat.berkeley.edu, Nils Arrigo (nils.arrigo@unine.ch), University Of Neuchatel, Laboratory of Evolutive Botany, 2009 Neuchatel, Switzerland http://www.unine.ch/bota/EBOLAB/}