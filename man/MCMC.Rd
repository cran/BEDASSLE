\name{MCMC}
\alias{MCMC}
\title{
	Runs the Markov chain Monte Carlo with the standard (Binomial) model
}
\description{
	This function initiates the Markov chain Monte Carlo (MCMC) for the binomial BEDASSLE
	model.
}
\usage{
MCMC(counts, sample_sizes, D, E, k, loci, delta, aD_stp, aE_stp, a2_stp, thetas_stp,
mu_stp, ngen,
printfreq, savefreq, samplefreq, directory = NULL, prefix = "", continue = FALSE,
continuing.params
= NULL)
}
\arguments{
  \item{counts}{
	A matrix of allelic count data, for which \code{nrow = } the number of populations
	and \code{ncol = } the number of bi-allelic loci sampled.  Each cell gives the number 
	of times allele `1' is observed in each population.  The choice of which allele is 
	allele `1' is arbitrary, but must be consistent across all populations at a locus.
}
  \item{sample_sizes}{
	A matrix of sample sizes, for which \code{nrow = } the number of populations
	and \code{ncol = } the number of bi-allelic loci sampled (i.e. - the dimensions of
	\code{sample.sizes} must match those of \code{counts}).  Each cell gives the number 
	of chromosomes successfully genotyped at each locus in each population.
}
  \item{D}{
	Pairwise geographic distance (\eqn{D_{i,j}}).  This may be Euclidean, or, if the
geographic
	scale of sampling merits it, great-circle distance.
}
  \item{E}{
	Pairwise ecological distance(s) (\eqn{E_{i,j}}), which may be continuous (e.g. - 
	difference in elevation) or binary (same or opposite side of some hypothesized 
	barrier to gene flow).  Users may specify one or more ecological distance matrices.  
	If more than one is specified, they should be formatted as a \code{list}.
}
  \item{k}{
	The number of populations in the analysis.  This should be equal to
\code{nrow(}counts\code{)}.
}
  \item{loci}{
	The number of loci in the analysis.  This should be equal to 
	\code{ncol(}counts\code{)}
}
  \item{delta}{
	The size of the "delta shift" on the off-diagonal elements of the parametric
	covariance matrix, used to ensure its positive-definiteness (even, for example,
	when there are separate populations sampled at the same geographic/ecological 
	coordinates).  This value must be large enough that the covariance matrix is 
	positive-definite, but, if possible, should be smaller than the smallest off-
	diagonal distance elements, lest it have an undue impact on inference.  If the 
	user is concerned that the delta shift is too large relative to the pairwise 
	distance elements in \code{D} and \code{E}, she should run subsequent analyses, 
	varying the size of delta, to see if it has an impact on model inference.
}
  \item{aD_stp}{
	The scale of the tuning parameter on aD (alphaD).  The scale of the tuning 
	parameter is the standard deviation of the normal distribution from which small
	perturbations are made to those parameters updated via a random-walk sampler.
	A larger value of the scale of the tuning parameter will lead to, on average, 
	larger proposed moves and lower acceptance rates (for more on acceptance rates,
	see \command{plot_acceptance_rate}).
}
  \item{aE_stp}{
	The scale of the tuning parameter on aE (alphaE).  If there are multiple 
	ecological distances included in the analysis, there will be multiple alphaE
	parameters (one for each matrix in the list of E).  These may be updated all 
	with the same scale of a tuning parameter, or they can each get their own, in 
	which case aE_stp should be a vector of length equal to the number of ecological
	distance variables.
}
  \item{a2_stp}{
	The scale of the tuning parameter on a2 (alpha_2).
}
  \item{thetas_stp}{
	The scale of the tuning parameter on the theta parameters.
}
  \item{mu_stp}{
	The scale of the tuning parameter on mu.
}
  \item{ngen}{
	The number of generations over which to run the MCMC (one parameter is updated 
	at random per generation, with mu, theta, and phi all counting, for the purposes of 
	updates, as one parameter).
}
  \item{printfreq}{
	The frequency with which MCMC progress is printed to the screen.  If 
	\code{printfreq =1000}, an update with the MCMC generation number and the posterior 
	probability at that generation will print to the screen every 1000 generations.
}
  \item{savefreq}{
	The frequency with which the MCMC saves its output as an R object (\code{savefreq =
50,000}
	 means that MCMC output is saved every 50,000 generations).  If \code{ngen} is large, 
	 this saving process may be computationally expensive, and so should not be performed 
	 too frequently.  However, users may wish to evalute MCMC performance while the chain 
	 is still running, or may be forced to truncate runs early, and should therefore 
	 specify a \code{savefreq} that is less than \code{ngen}.  We recommend a
\code{savefreq}
	 of between 1/10th and 1/20th of \code{ngen}.
}
  \item{samplefreq}{
	The thinning of the MCMC chain (\code{samplefreq = 1000} means that the parameter 
	values saved in the MCMC output are sampled once every 1000 generations).  A higher 
	\code{samplefreq} will decrease parameter autocorrelation time.  However, there is 
	still information in autocorrelated draws from the joint posterior, so the
\code{samplefreq} 
	should be viewed merely as a computational convenience, to decrease the size of the 
	MCMC output objects.
}
  \item{directory}{
	If specified, this points to a directory into which output will be saved.
}
  \item{prefix}{
	If specified, this prefix will be added to all output file names.
}
  \item{continue}{
	If \code{TRUE}, this will initiate the MCMC chain from the last parameter values of a 
	previous analysis.  This option can be used to effectively increase the \code{ngen} 
	of an initial run.  If \code{FALSE}, the MCMC will be initiated from random parameter
	values.
}
  \item{continuing.params}{
	The list of parameter values used to initiate the MCMC if \code{continue = TRUE}.  If 
	the user wants to continue an analysis on a dataset, these should be the parameter 
	values from the last generation of the previous analysis.  This list may be generated 
	using the function \command{make.continuing.params}.
}
}
\details{
	This function saves an MCMC output object at intervals specified by \code{savefreq}.  
	This object may be ported into R working memory using the \emph{base} function 
	\command{load}.

	As with any MCMC method, it is very important here to perform MCMC diagnosis and 
	evaluate chain mixing.  I have provided a number of MCMC diagnosis graphing functions 
	for user convenience in visually assessing MCMC output.  These include 
	\command{plot_all_trace};\command{plot_all_marginals};
	\command{plot_all_joint_marginals};
	and \command{plot_all_acceptance_rates}.  To evaluate model adequacy, users should 
	use \command{posterior.predictive.sample} and 
	\command{plot_posterior_predictive_sample}.  These MCMC diagnosis/model adequacy 
	functions all call the standard MCMC output R object that the BEDASSLE MCMC generates 
	as their principal argument.
	
	If users wish to start another MCMC run from where the current run left off, they
	should use \command{make.continuing.params}, and initiate the new run with option 
	\code{continue = TRUE} and the \code{continuing.params} list from the previous run
	specified.
}
\references{
Bradburd, G.S., Ralph, P.L., and Coop, G.M. Disentangling the effects of geographic and
ecological
isolation on genetic differentiation. \emph{Evolution} 2013.
}
\author{
Gideon Bradburd
}
\examples{
#With the HGDP dataset and mcmc operators
	data(HGDP.bedassle.data)
	data(mcmc.operators)

#Call the Markov chain Monte Carlo for the standard model	
	MCMC(
		counts = HGDP.bedassle.data$allele.counts,
		sample_sizes = HGDP.bedassle.data$sample.sizes,
		D = HGDP.bedassle.data$GeoDistance,
		E = HGDP.bedassle.data$EcoDistance,
		k = HGDP.bedassle.data$number.of.populations,
		loci = HGDP.bedassle.data$number.of.loci,
		delta = mcmc.operators$delta,
		aD_stp = mcmc.operators$aD_stp,
		aE_stp = mcmc.operators$aE_stp,
		a2_stp = mcmc.operators$a2_stp,
		thetas_stp = mcmc.operators$thetas_stp,
		mu_stp = mcmc.operators$mu_stp,
		ngen = mcmc.operators$ngen,
		printfreq = mcmc.operators$printfreq,
		savefreq = mcmc.operators$savefreq,
		samplefreq = mcmc.operators$samplefreq,
		directory = NULL,
		prefix = mcmc.operators$prefix,
		continue = FALSE,
		continuing.params = NULL
	)
}