\name{posterior.predictive.sample}
\alias{posterior.predictive.sample}
\title{
	Generates posterior predictive samples
}
\description{
	This function simulates data using the inference model parameterized from the joint
	posterior of the MCMC and the observed independent variables 
	(\eqn{D_{ij} and E_{ij}}).  These posterior predictive samples can be compared to the
	 observed data to see how well the model is able to describe the observed data.
}
\usage{
posterior.predictive.sample(MCMC.output, posterior.predictive.sample.size, output.file, 
prefix = "")
}
\arguments{
  \item{MCMC.output}{
	The standard MCMC output file generated from a BEDASSLE run.
}
  \item{posterior.predictive.sample.size}{
	The number of posterior predictive datasets the user wishes to simulate.
}
  \item{output.file}{
	The name that will be assigned to the R object containing the posterior predictive 
	datasets.  The suffix ".Robj" will be appended to the user-specified name.
}
  \item{prefix}{
	If specified, this prefix will be added to the output file name.
}
}
\details{
	This function simulates datasets like those the user analyzed with BEDASSLE, using
	the same independent variables (\code{sample.sizes}, \eqn{D_{ij}} and \eqn{E_{ij}}) 
	as in the user's dataset and plugging them into the inference model, which is 
	parameterized by randomly drawing parameter values from the joint posterior output of 
	the MCMC analysis.  These posterior predictive simulated allelic count data are 
	summarized as unbiased pairwise \eqn{F_{ST}} 
	(using \command{calculate.all.pairwise.Fst}), which may then be compared to the 
	observed unbiased pairwise \eqn{F_{ST}} to determine how well the model is able to 
	describe the user's data.  The output of \command{posterior.predictive.sample} can be 
	visualized using \command{plot.posterior.predictive.sample}.
}
\author{
Gideon Bradburd
}