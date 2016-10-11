\name{EXTRACTAFLP}
\alias{EXTRACTAFLP}
\alias{EXTRACTAFLP.PROCESS}
\title{ Merges Observed Peaks into 'Bins' }
\description{
Automatization of fragments scoring, needs to affiliate the observed peaks into groups ('bins'), 
and then to score the presence/absence of each bin for each sample.
A bin contains every sizes for a given DNA fragment across every samples. 
This function allows the user to set the 'width', the way to split bins. 
Bins are then filtered according to quality criteria. 
Please consult RawGeno's manual in the "doc" folder.
}

\usage{
EXTRACTAFLP(all.dat, samples.names, TOL = 1
            , MAXBIN = max(all.dat$Size), MINBIN = 0.2
            , probs = c(5e-04), freq = 1, who = "ReplicateID"
            ,thresh = 95,keep = T)
            }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{all.dat}{ Dataframe produced by \code{OPENAFLP}, containing:
                  One line per peak, lines are sorted according to peak size (in bp).
                  colnames(all.dat)='sample.id','Size','Peak_height', where: 
                  1. sample.id - Code defining peak's to sample affiliation
                  2. Size - Peak's size in bp
                  3. Peak_height - Peak's height  }
  \item{samples.names}{ Vector produced by \code{OPENAFLP}, containing
                        the name of GeneScan output files. }
  \item{TOL}{ Tolerance Index, ranges from 0 to 1. Defines the way to split bins. 
              Consult \code{\link{msc.peaks.clust}}.}
  \item{MAXBIN}{ Maximum width of bins (bp). }
  \item{MINBIN}{ Minimum width of bins (bp). }
  \item{freq}{ Bin exclusion threshold, according to its frequency in the dataset (minimum number of occurences). }
  \item{who}{ Bin reproducibility test. In case of replicates included in the dataset. ReplicateID identifies the replicated sample (see documentation). }
  \item{thresh}{ Bin reproducibility test, exclusion threshold (in percents). Bins that are reproducible in at least "thresh" percents of the replicated samples, are conserved. }
  \item{keep}{ Bin reproducibility test, should untested bins be keeped in the dataset? }
  \item{cutRFU}{ Bin exclusion threshold based on mean bin intensity. Refer to Whitlock et al. 2008. This filter is still under development!}
  \item{interspace}{ Ranges from 0 to 1 (quantile). Bins that are closely sized can more likely contain mis-affiliated peaks. 
                     This option allows to define a minimum size interval 
                     (defined by using quantiles of the size bins intervals). }
  \item{RMIN}{ Upper limit of the scoring range (bp). }
  \item{RMAX}{ Lower limit of the scoring range (bp). }
  \item{probs}{ Peak exclusion threshold from a bin (quantile), according to its intensity (see documentation). }
   
}
\details{
Migration of DNA fragments during Genescan generally slightly varies. 
Those variations are related to the quality of PCR and migration conditions (e.g. resolution and age of the capillary, room temperature...)
Indeed, a same DNA fragment (represented by a peak in a genescan), can be detected at different positions 
from one individual to another.

The analysis is a four steps process: 
I. Scoring phase, using the \code{\link{geno.clust}} function (wihtin \code{\link{EXTRACTAFLP}}), 
II. Filtering bins and peaks according to different quality criteria (within \code{\link{EXTRACTAFLP}}), 
III. Controling quality of bins (\code{\link{QUALDIAG}} and \code{\link{PLOT.PSEUDO.PROCESS}}),
IV. Exporting the result to other analysis platforms (\code{\link{SAVE.GUI}} function, supervising the AFLPdat functions collection - Ehrich, 2007)

Those parameters must be set up according to the resolution of the capillary of the sequencer. 
Increasing the width of bins increases artificial homoplasy, as several different within the same sample
fragments may be affiliated to the same bin. Please, consult diagnostic graphics.
}

\value{
A list (data.binary) containing:
  \itemize{
  \item{data.binary}{A binary matrix (individuals x bins), colnames(data.binary$data.binary)=GeneScan files' names.}
  \item{peaks.info}{bins informations added to the all.dat dataframe.}
  \item{data.hom}{Corresponds to data.binary, but contains homoplasy information.}
  \item{data.height}{Corresponds to data.binary, but contains peaks height information.}
  \item{data.size}{Corresponds to data.binary, but contains bin sizes information.}
  \item{table.stats}{Contains reproducibility and other scoring statistics.}
  \item{data.height}{Contains scoring parameters.}
 }                                    
}

\references{ The scoring algorithm (\code{\link{geno.clust}}) was developped by the Virginia Prostate Center, and adapted by Jarek Tuszynski (\code{\link{msc.peaks.clust}}, caMassClass).
 }
\author{ Nils Arrigo (nils.arrigo@unine.ch), University Of Neuchatel, Laboratory of Evolutive Botany, 2009 Neuchatel, Switzerland 
         http://www.unine.ch/bota/EBOLAB/}
\examples{}
\keyword{ Cluster }
\keyword{ Peaks }
\keyword{ AFLP }
\keyword{ Genotyping }
\keyword{ Genescan }
\keyword{ Binary }
\keyword{ Allele }
\keyword{ Presence }
\keyword{ Absence }
