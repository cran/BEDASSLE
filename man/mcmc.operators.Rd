\name{mcmc.operators}
\alias{mcmc.operators}
\docType{data}
\title{
	Operator parameters that control the operation of the MCMC
}
\description{
	These parameters, which are passed to the \command{MCMC} and \command{MCMC_BB} 
	functions, control the operation of the MCMC.  They specify the number of 
	generations over which the MCMC runs; the scales of the tuning parameters (stp)
	for all parameters updated via random-walk samplers; the save, print, and 
	sample frequency of the chain, and the output file names.
}
\usage{data(mcmc.operators)}
\format{
  The format is:
List of 12
	\describe{
		 \item{$ delta     :}{ num 0.001}
		 \item{$ aD_stp    :}{ num 0.0018}
		 \item{$ aE_stp    :}{ num 0.04}
		 \item{$ a2_stp    :}{ num 0.0035}
		 \item{$ phi_stp   :}{ num 30}
		 \item{$ thetas_stp:}{ num 0.07}
		 \item{$ mu_stp    :}{ num 0.17}
		 \item{$ ngen      :}{ num 100}
		 \item{$ printfreq :}{ num 2}
		 \item{$ savefreq  :}{ num 100}
		 \item{$ samplefreq:}{ num 5}
		 \item{$ prefix    :}{ chr "example_"}
	}
}
\details{
	\describe{
		\item{delta}{The size of the "delta shift" on the off-diagonal elements of the 
			parametric covariance matrix, used to ensure its positive-definiteness (even, 
			for example, when there are separate populations sampled at the same 
			geographic/ecological coordinates).  This value must be large enough that the 
			covariance matrix is positive-definite, but, if possible, should be smaller 
			than the smallest off-diagonal distance elements, lest it have an undue 
			impact on inference.  If the user is concerned that the delta shift is too 
			large relative to the pairwise distance elements in \code{D} and \code{E}, 
			she should run subsequent analyses, varying the size of delta, to see if it 
			has an impact on model inference.}
		\item{aD_stp}{The scale of the tuning parameter on aD (alphaD).  The scale of the 
			tuning parameter is the standard deviation of the normal distribution from 
			which small perturbations are made to those parameters updated via a 
			random-walk sampler. A larger value of the scale of the tuning parameter will 
			lead to, on average, larger proposed moves and lower acceptance rates (for 
			more on acceptance rates, see \command{plot_acceptance_rate}).}
		\item{aE_stp}{The scale of the tuning parameter on aE (alphaE).  If there are 
			multiple ecological distances included in the analysis, there will be 
			multiple alphaE parameters (one for each matrix in the list of E).  These may 
			be updated all with the same scale of a tuning parameter, or they can each 
			get their own, in which case aE_stp should be a vector of length equal to the 
			number of ecological distance variables.}
		\item{a2_stp}{The scale of the tuning parameter on a2 (alpha_2).}
		\item{phi_stp}{The scale of the tuning parameter on the phi parameters.}
		\item{thetas_stp}{The scale of the tuning parameter on the theta parameters.}
		\item{mu_stp}{The scale of the tuning parameter on mu.}
		\item{ngen}{The number of generations over which to run the MCMC (one parameter 
			is updated at random per generation, with mu, theta, and phi all counting, 
			for the purposes of updates, as one parameter).}
		\item{printfreq}{	The frequency with which MCMC progress is printed to the 
			screen.  If \code{printfreq =1000}, an update with the MCMC generation number 
			and the posterior probability at that generation will print to the screen 
			every 1000 generations.}
		\item{savefreq}{The frequency with which the MCMC saves its output as an R 
			object (\code{savefreq =50,000} means that MCMC output is saved every 50,000 
			generations).  If \code{ngen} is large, this saving process may be 
			computationally expensive, and so should not be performed too frequently.  
			However, users may wish to evalute MCMC performance while the chain is still 
			running, or may be forced to truncate runs early, and should therefore 
			specify a \code{savefreq} that is less than \code{ngen}.  We recommend a 
			\code{savefreq} of between 1/10th and 1/20th of \code{ngen}.}
		\item{samplefreq}{The thinning of the MCMC chain (\code{samplefreq = 1000} means 
			that the parameter values saved in the MCMC output are sampled once every 
			1000 generations).  A higher \code{samplefreq} will decrease parameter 
			autocorrelation time.  However, there is still information in autocorrelated 
			draws from the joint posterior, so the \code{samplefreq} should be viewed 
			merely as a computational convenience, to decrease the size of the MCMC 
			output objects.}
		\item{prefix}{If specified, this prefix will be added to all output file names.}
	}
}
\examples{
## see \command{MCMC} and \command{MCMC_BB} for example usage.
}
\keyword{datasets}
