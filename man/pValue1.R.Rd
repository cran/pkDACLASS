\name{pValue1.R}
\alias{pValue1.R}
\title{
Bootstrap Pvlaues for the Monoisotopic peak test
}
\description{
pValue1.R calculates the bootstrapped p-values for the Kulback-Leibler goodness of fit test on intensities. The null hypothesis is that the intensities follow a location-shifted Poisson model.
}
\usage{
pValue1.R(x, par, B)
}
\arguments{
  \item{x}{
Real data set, a dataframe of two columns; the first column the m/z ratios and the second column, their corresponding intensities.
}
  \item{par}{
Estimated parameters from the EM algorithm.
}
  \item{B}{
Integer; the bootstrap sample size.
}
}
\details{
The function pValue1.R returns the bootstrap p-values based on the Kullback-Leibler test. The function uses the parameter estimates from the EM algorithm and the bootstrap sample to calculate the p-values. 
}
\value{
The values returned are the p-values based on the Kullback-Leibler test.
}
\references{
Atlas, M and Datta, S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2:202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

