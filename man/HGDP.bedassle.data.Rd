\name{HGDP.bedassle.data}
\alias{HGDP.bedassle.data}
\docType{data}
\title{
	The Eurasian subset of the HGDP dataset used in example BEDASSLE analyses 
}
\description{
	The allelic counts, sample sizes, geographic distances, ecological distances, and 
	population metadata from the 38 human populations used in example BEDASSLE 
	analyses, subsetted from the Human Genome Diversity Panel (HGDP) dataset.
}
\usage{data(HGDP.bedassle.data)}
\format{
  The format is:
List of 7
	\describe{
		 \item{$ allele.counts        :}{int [1:38, 1:1000] 12 16 5 17 4 14 20 5 34 ...}
			\itemize{
			  \item{..- attr(*, "dimnames")=List of 2}
			  \item{.. ..$ : chr [1:38] "Adygei" "Basque" "Italian" "French" ...}
			  \item{.. ..$ : chr [1:1000] "rs13287637" "rs17792496" "rs1968588" ...}
			}
		 \item{$ sample.sizes         :}{int [1:38, 1:1000] 34 48 24 56 30 50 56 ...}
			\itemize{
			  \item{..- attr(*, "dimnames")=List of 2}
			  \item{.. ..$ : chr [1:38] "Adygei" "Basque" "Italian" "French" ...}
			  \item{.. ..$ : chr [1:1000] "rs13287637" "rs17792496" "rs1968588" ...}
			}
		 \item{$ GeoDistance          :}{num [1:38, 1:38] 0 1.187 0.867 1.101 1.247 ...}
		 \item{$ EcoDistance          :}{num [1:38, 1:38] 0 0 0 0 0 0 0 0 0 0 ...}
			\itemize{
			  \item{..- attr(*, "dimnames")=List of 2}
			  \item{.. ..$ : chr [1:38] "1" "2" "3" "4" ...}
			  \item{.. ..$ : chr [1:38] "1" "2" "3" "4" ...}
		  	}
		 \item{$ number.of.populations:}{int 38}
		 \item{$ number.of.loci       :}{int 1000}
		 \item{$ hgdp.metadata        :}{'data.frame':	38 obs. of  3 variables:}
			\describe{
			  \item{..$ Population:}{ chr [1:38] "Adygei" "Basque" "Italian" ...}
			  \item{..$ Latitude  :}{ chr [1:38] "44" "43" "46" "46" ...}
			  \item{..$ Longitude :}{ chr [1:38] "39" "0" "10" "2" ...}
			}
	}
}
\details{
	\describe{
		\item{allele.counts}{A matrix of allelic count data, for which \code{nrow = } 
			the number of populations and \code{ncol = } the number of bi-allelic loci 
			sampled.  Each cell gives the number of times allele `1' is observed in each 
			population.  The choice of which allele is allele `1' is arbitrary, but must 
			be consistent across all populations at a locus.}
		\item{sample.sizes}{A matrix of sample sizes, for which \code{nrow = } the number 
			of populations and \code{ncol = } the number of bi-allelic loci sampled 
			(i.e. - the dimensions of \code{sample.sizes} must match those of 
			\code{counts}).  Each cell gives the number of chromosomes successfully 
			genotyped at each locus in each population.}
		\item{Geo.Distance}{Pairwise geographic distance (\eqn{D_{i,j}}).  This may be 
			Euclidean, or, if the geographic scale of sampling merits it, great-circle 
			distance.  In the case of this dataset, it is great-circle distance.}
		\item{Eco.Distance}{Pairwise ecological distance(s) (\eqn{E_{i,j}}), which may 
			be continuous (e.g. - difference in elevation) or binary (same or opposite 
			side of some hypothesized barrier to gene flow).  In this case, the 
			ecological distance is binary, representing whether a pair of populations 
			occurs on the same side, or on opposite sides, of the Himalayas.}
		\item{number.of.populations}{The number of populations in the analysis.  
			This should be equal to \code{nrow(}counts\code{)}.  In this dataset, there
			are 38 populations sampled.}
		\item{number.of.loci}{The number of loci in the analysis.  This should be equal 
			to \code{ncol(}counts\code{)}.  In this dataset, there are 1000 loci 
			sampled.}
		\item{hgdp.metadata}{This data frame contains the metadata on the populations
			included in the analysis, including:
				\itemize{
					\item{Population name}
					\item{Latitude}
					\item{Longitude}
				}
		}
	}
}
\source{
	\itemize{
		\item{Conrad et al. A worldwide survey of haplotype variation and linkage
			disequilibrium in the human genome. \emph{Nature Genetics} 2008.}
		\item{Li et al. Worldwide human relationships inferred from genome-wide 
			patterns of variation. \emph{Science} 2008.}
	}	
}
\references{
	Bradburd, G.S., Ralph, P.L., and Coop, G.M. Disentangling the effects of 
		geographic and ecological isolation on genetic differentiation. 
		\emph{Evolution} 2013.
}
\examples{
## see \command{MCMC}, \command{MCMC_BB}, \command{calculate.pariwise.Fst}, 
## \command{calculate.all.pairwise.Fst}, and \command{Covariance} for usage.
}
\keyword{datasets}
