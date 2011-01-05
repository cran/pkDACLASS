\name{pkpk.R}
\alias{pkpk.R}
\title{
Peak Picking Algorithm
}
\description{
pkpk.R matches the peaks in the denoised data with the corresponding intensities in the set of possible cluster of features.
}
\usage{
pkpk.R(dat1, dat2, h)
}
\arguments{
  \item{dat1}{
A set of possible cluster of features.
}
  \item{dat2}{
Real data, a data frame with two columns for which the first column are the m/z values and the second column their corresponding intensities. 
}
  \item{h}{
Height, threshold value where to cut the intensities.
}
}
\details{
The function pkpk.R takes as input a set of possible cluster of features, a real dataset, a cutoff point (h) and returns a dataframe of the unique monoisotopic peaks identified.
}
\value{
A dataframe of two colums, the first column are the m/z values and the second column their corresponding intensities for the unique monisotopic peaks identified.
}
\references{
Atlas M, Datta S (2009) A statistical Technique for Monoisotopic Peak Detectation in a Mass Spectrum. J Proteomics Bioinform 2: 202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta.
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

