\name{realData.R}
\alias{realData.R}
\title{
Real Data Analysis
}
\description{
A method for monoisotopic peak detection based on a number of steps which are preprocessing, and monoisotopic peak detection. Preprocessing comprises baseline correction, denoising, and binning. Monoisotopic peak detection consists of extraction of cluster of features, model fitting and the monoisotopic peak test.
}
\usage{
realData.R(d, h, alpha, B, INDEX)
}
\arguments{
  \item{d}{
Real data set, a dataframe of two columns; the first column being m/z ratios and the second column their corresponding intensities.
}
  \item{h}{
The height, threshold to cut intensities.
}
  \item{alpha}{
The nominal level.
}
  \item{B}{
Integer, the bootstrap sample size.
}
  \item{INDEX}{
Rounds m/z values.
}
}
\details{
The function realData.R takes as input a real data set, a nominal level-alpha, a bootstrap sample size (B) and a cut off value (h) for the intensities and returns a monoisotopic peak of mz values and their corresponding intensities. The analysis goes through a number of steps namely; baseline correction, denoising, binning, extraction of possible cluster of features, reduction of the isotopic distribution, model fitting & estimate the parameters of the model by the EM algorithm, performing hypothesis testing on the intensities that they come from a location-shifted Poisson model, and finally draw conclusions.
}
\value{
\item{data }{set of features of contiguous peaks}
  \item{pval }{Bootstrap p-values}
 \item{mono.mz }{mass to charge values}
\item{mono.y }{corresponding intensities of m/z values}
}
\references{
Atlas M, Datta S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2: 202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

