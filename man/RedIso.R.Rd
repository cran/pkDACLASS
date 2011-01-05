\name{RedIso.R}
\alias{RedIso.R}
\title{
Reducing the Isotopic Distribution
}
\description{
RedIso.R is a function that takes as input data set, an extracted set of clusters of features and their intensities, and returns an isotopic distribution in form of a reduced set of cluster of features of a given size. 
}
\usage{
RedIso.R(dat, c = 3, ...)
}
\arguments{
  \item{dat}{
The isotopic distribution of the data, an array for which each element is a dataframe of two columns; the first column are the m/z values and the second column their corresponding intensities.
}
  \item{c}{
An integer taken to be the smallest length of an isotopic distribution.
}
  \item{\dots}{
Additional arguments to pass to the isotopic reducing function.
}
}
\details{
The function evaluates features for a given data set, and returns a reduced cluster of features of a given size. The size is chosen such that it is greater than the total number of parameters in the assumed model. 
}
\value{
A data set of cluster of features of a given size. An array for which each element is a dataframe of two columns; the first column are the m/z values and the second column their corresponding intensities.
}
\references{
Atlas M, Datta S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2: 202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

