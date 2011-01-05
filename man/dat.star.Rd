\name{dat.star}
\alias{dat.star}
\title{
Generating Bootstrap Samples 
}
\description{
The function dat.star returns bootstrapped samples based on a location-shifted Poisson model which will later be used for testing the monoisotopic peak. 
}
\usage{
dat.star(dat, pars, B, ...)
}
\arguments{
  \item{dat}{
Extracted cluster of features.
}
  \item{pars}{
The parameter estimates from the data based on the assumed model. 
}
  \item{B}{
Integer, bootstrap sample size.
}
  \item{\dots}{
Additional arguments to pass through the bootstrap resampling function.
}
}
\details{
The function dat.star generates bootstrapped samples, based on parameters estimated by the EM algorithm. The underlying model is location-shifted Poisson distribution. 
}
\value{
Bootstrapped samples, (which are clusters of features) of size sum of frequencies. Each cluster is matrix of two columns, column one the m/z values and column two their corresponding intensities.
}
\references{
Atlas, M and Datta, S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2:202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

