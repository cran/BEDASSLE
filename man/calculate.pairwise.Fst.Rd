\name{calculate.pairwise.Fst}
\alias{calculate.pairwise.Fst}
\title{
	Calculates unbiased pairwise Fst between a pair of populations
}
\description{
	This function calculates unbiased \eqn{F_{ST}} (based on Weir and Hill's 
	\eqn{\theta}, 2002), between a pair of populations/individuals. Loci for which either 
	of the populations/individuals has missing data (i.e. - the sample size is zero) are 
	excluded.  
}
\usage{
calculate.pairwise.Fst(allele.counts, sample.sizes)
}
\arguments{
  \item{allele.counts}{
	A matrix of allele counts of dimensions \code{nrow = } 2, one for each of the two 
	populations/individuals between which pairwise \eqn{F_{ST}} is being calculated, and 
	\code{ncol = } the number of loci genotyped.
}
  \item{sample.sizes}{
	A matrix of sample sizes of dimensions \code{nrow = } 2, one for each of the two 
	populations/individuals between which pairwise \eqn{F_{ST}} is being calculated, and 
	\code{ncol = } the number of loci genotyped (i.e. - the dimensions of
	\code{sample.sizes} must match those of \code{counts}).  Each cell gives the number 
	of chromosomes successfully genotyped at each locus in each population.
}
}
\value{
	Pairwise unbiased \eqn{F_{ST}} between a pair of populations/individuals
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

#Draw 2 populations at random from the Eurasian HGDP dataset
	pop1 <- sample(nrow(HGDP.bedassle.data$allele.counts),1)
	pop2 <- sample(nrow(HGDP.bedassle.data$allele.counts),1)

#Calculate unbiased Fst between them
	pairwise.Fst <- calculate.pairwise.Fst(
		HGDP.bedassle.data$allele.counts[c(pop1,pop2),],
		HGDP.bedassle.data$sample.sizes[c(pop1,pop2),]	
	)

#Print that Fst to the console
	print(sprintf("Fst between the \%s population and the \%s population is \%s",
		HGDP.bedassle.data$hgdp.metadata[pop1,1],
		HGDP.bedassle.data$hgdp.metadata[pop2,1],
		round(pairwise.Fst,3))
	 )
}
