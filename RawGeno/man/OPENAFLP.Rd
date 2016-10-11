\name{OPENAFLP}
\alias{OPENAFLP}
\alias{OPENAFLP.PROCESS}
\alias{OPENAFLP.PROCESS.BATCH}
\title{ Import GeneScan Outputs }
\description{
  The function imports GeneScan outputs (see examples for the format to use), 
  and merges them in a single dataframe. Large sets of files can be imported. 
  Uses a TCL Graphical User Interface (\link{OPENAFLP.PROCESS}), but command lines may also be used.
}
\usage{
OPENAFLP(listfiles, dyecol = "B")
}
\arguments{
  \item{listfiles}{ A vector containing the list of paths for genescan outputs to be imported.
                    Produced internally in \link{OPENAFLP.PROCESS} with the function 'choose.files' (utils) }
  \item{dyecol}{ A vector defining the Dye Color to be considered. }
  \item{pksc}{ Indicates if the electropherograms to import were analysed wth GeneScan (pksc=F) or PeakScanner (pksc=T)}
}
\details{
The function works with outputs produced by ABI Genscan 3.7 ( ABI Prism), by exporting 
chromatograms in a *.txt file (File>Export table).
Outputs must be saved in tab-delimited format, and contain the following informations:
\code{1. DyeSample_peak}
\code{2. Minutes}
\code{3. Size}
\code{4. Peak_height}
\code{5. Peak_area}
\code{6. Data_point}
Please, consult examples, OPENAFLP is sensitive to columns order.
}
\value{
A list named AFLP, containing:
  \item{all.dat}{Dataframe containing the whole imported dataset, 
                  arranged in a proper format, in order to facilitate work with \link{EXTRACTAFLP}.
                  The dataframe contains one line per peak, lines are sorted according to peak size (in bp).
                  colnames(all.dat)='sample.id','Size','Peak_height', where: 
                  \code{1. sample.id} {Code defining peak's to sample affiliation}
                  \code{2. Size} {Peak's size in bp}
                  \code{3. Peak_height} {Peak's height}
                  }
  \item{samples.names}{Vector containing the name of GeneScan output files. 
                       This information will be used by \link{EXTRACTAFLP} to 
                       give correct names in the binary table.}
}
\references{ ~put references to the literature/web site here ~ }
\author{ Nils Arrigo (nils.arrigo@unine.ch), University Of Neuchatel, Laboratory of Evolutive Botany, 2009 Neuchatel, Switzerland 
         http://www.unine.ch/bota/EBOLAB/}
\seealso{ \code{\link{RawGeno}},\code{\link{EXTRACTAFLP}}, }
\examples{}

\keyword{ Import  }
\keyword{ Genescan }
\keyword{ Genotype }
\keyword{ AFLP }
\keyword{ Dominant }
\keyword{ Binary }
\keyword{ Genotyping }
