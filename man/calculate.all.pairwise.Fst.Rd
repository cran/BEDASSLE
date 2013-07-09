\name{calculate.all.pairwise.Fst}
\alias{calculate.all.pairwise.Fst}
\title{
	Calculates unbiased pairwise Fst between all sampled populations
}
\description{
This function calculates unbiased \eqn{F_{ST}} (based on Weir and Hill's \eqn{\theta}, 
2002), between all populations/individuals included in the \code{counts} matrix, and 
returns the results in a \code{k} by \code{k} matrix, where \code{k = nrow(counts)}.  
Loci for which either of the populations/individuals has missing data (i.e. - the sample 
size is zero) are excluded.
}
\usage{
calculate.all.pairwise.Fst(allele.counts, sample.sizes)
}
\arguments{
  \item{allele.counts}{
	A matrix of allelic count data, for which \code{nrow = } the number of populations
	and \code{ncol = } the number of bi-allelic loci sampled.  Each cell gives the number 
	of times allele `1' is observed in each population.  The choice of which allele is 
	allele `1' is arbitrary, but must be consistent across all populations at a locus.
}
  \item{sample.sizes}{
	A matrix of sample sizes, for which \code{nrow = } the number of populations
	and \code{ncol = } the number of bi-allelic loci sampled (i.e. - the dimensions of
	\code{sample.sizes} must match those of \code{counts}).  Each cell gives the number 
	of chromosomes successfully genotyped at each locus in each population.
}
}
\value{
	A matrix of pairwise, unbiased \eqn{F_{ST}}.
}
\references{
	Weir,B.S. and W.G. Hill. 2002. Estimating F-statistics. Ann.Rev.Gen. 36:949-952.
}
\author{
	Gideon Bradburd
}
\examples{
#With the HGDP dataset
	data(HGDP.bedassle.data)

#Calculate pairwise Fst between all population pairs
	hgdp.pairwise.Fst <- calculate.all.pairwise.Fst(
		HGDP.bedassle.data$allele.counts,
		HGDP.bedassle.data$sample.sizes
	)	

#Plot pairwise Fst against geographic distance
	plot(HGDP.bedassle.data$GeoDistance,
		hgdp.pairwise.Fst,
		pch=19,
		col=HGDP.bedassle.data$EcoDistance+1,
		ylab="pairwise Fst",
		xlab="geographic distance",
		main="isolation by distance")
			legend(x="bottomright",pch=19,col=c(1,2),
				legend=c("same side of Himalayas",
						"opposite sides of Himalayas"))
}
