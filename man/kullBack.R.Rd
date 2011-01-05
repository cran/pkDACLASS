\name{kullBack.R}
\alias{kullBack.R}
\title{
Kullback-Leibler divergence measure
}
\description{
The function kullback.R calculates the goodness of fit test statistic based on the Kullback-Leibler divergence measure as described by Kullback and Leibler (1951) which is used for testing the isotopic distribution.
}
\usage{
kullBack.R(dat, par)
}
\arguments{
  \item{dat}{
Real or simulated data, usually a matrix of two columns; column one are the m/z values and column two are the corresponding intensities.
}
  \item{par}{
Parameter estimates from the EM algorithm
}
}
\details{
The function kullback.R computes the goodness of fit test statistic based on the Kullback-Leibler divergence measure. 
}
\value{
kullBack.R returns the test statistic based on the Kullback-Leibler measure.
}
\references{
Kullback S and Leibler RA (1951) On information and sufficiency. Annals of Mathematical Statistics 22:79-86.

Atlas, M and Datta, S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2:202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line



